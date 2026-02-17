#' @include provider.R
NULL

#' Chat with Codex via the local Codex CLI
#'
#' @description
#' `chat_codex()` creates an ellmer chat backed by the local `codex` binary.
#' This provider uses the Codex app-server runtime instead of a direct HTTP API.
#'
#' @param system_prompt A system prompt to set the behavior of the assistant.
#' @param model Model to use for Codex turns.
#' @param params Common model parameters, usually created by [params()].
#' @param codex_bin Path to the local `codex` binary.
#' @param config Optional list of Codex app-server config overrides to pass to
#'   `thread/start`.
#' @param echo One of the following options:
#'   * `none`: don't emit any output (default when running in a function).
#'   * `output`: echo text and tool-calling output as it streams in (default
#'     when running at the console).
#'   * `all`: echo all input and output.
#'
#'   Note this only affects the `chat()` method.
#' @family chatbots
#' @export
#' @returns A [Chat] object.
chat_codex <- function(
  system_prompt = NULL,
  model = "gpt-5.3-codex",
  params = NULL,
  codex_bin = "codex",
  config = NULL,
  events = c("status", "none", "raw"),
  echo = c("none", "output", "all")
) {
  if (!is.null(config) && !is.list(config)) {
    cli::cli_abort("{.arg config} must be a list or NULL.")
  }
  events <- arg_match(events)
  echo <- check_echo(echo)

  provider <- ProviderCodex(
    name = "Codex",
    base_url = "codex://app-server",
    model = model,
    params = params %||% params(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL,
    codex_bin = codex_bin,
    config = config,
    events = events,
    runtime = codex_runtime_new()
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderCodex <- new_class(
  "ProviderCodex",
  parent = Provider,
  properties = list(
    codex_bin = prop_string(),
    config = class_any,
    events = prop_string(),
    runtime = class_any
  )
)

.codex_runtime_registry <- local({
  x <- new.env(parent = emptyenv())
  x$runtimes <- new.env(parent = emptyenv())
  x
})

method(chat_perform_provider, ProviderCodex) <- function(
  provider,
  mode = c("value", "stream", "async-stream", "async-value"),
  turns,
  tools = NULL,
  type = NULL
) {
  mode <- arg_match(mode)
  if (mode %in% c("async-stream", "async-value")) {
    cli::cli_abort(
      "{.fn chat_codex} does not yet support async chat methods.",
      class = "ellmer_codex_async_not_supported"
    )
  }

  input <- codex_turn_input(turns[[length(turns)]])
  output_schema <- if (is.null(type)) NULL else as_json(provider, type)

  if (mode == "value") {
    codex_run_turn(
      provider,
      input,
      tools = tools,
      output_schema = output_schema
    )
  } else {
    codex_stream_turn(
      provider,
      input,
      tools = tools,
      output_schema = output_schema,
      emit_events = TRUE
    )
  }
}

method(chat_response_body, ProviderCodex) <- function(provider, response) {
  response
}

method(chat_response_duration, ProviderCodex) <- function(provider, response) {
  response$duration
}

method(stream_content, ProviderCodex) <- function(provider, event) {
  if (is.null(event)) {
    return(NULL)
  }
  if (identical(event$type, "turn/completed")) {
    return(ContentText(event$result$text %||% ""))
  }
  if (!identical(event$type, "item/agentMessage/delta")) {
    return(NULL)
  }
  if (identical(provider@events, "status")) {
    return(NULL)
  }
  ContentText(event$delta)
}

method(stream_merge_chunks, ProviderCodex) <- function(
  provider,
  result,
  chunk
) {
  if (is.null(chunk) || !identical(chunk$type, "turn/completed")) {
    return(result)
  }
  chunk$result
}

method(value_turn, ProviderCodex) <- function(
  provider,
  result,
  has_type = FALSE
) {
  contents <- if (has_type) {
    list(ContentJson(string = result$text %||% ""))
  } else {
    list(ContentText(result$text %||% ""))
  }

  AssistantTurn(
    contents = contents,
    json = list(),
    tokens = unlist(tokens()),
    duration = result$duration %||% NA_real_
  )
}

codex_turn_input <- function(turn) {
  is_text <- map_lgl(turn@contents, S7_inherits, ContentText)
  if (!any(is_text)) {
    cli::cli_abort("{.fn chat_codex} currently only supports text user input.")
  }

  lapply(turn@contents[is_text], function(content) {
    list(type = "text", text = content@text)
  })
}

codex_stream_turn <- function(
  provider,
  input,
  tools = NULL,
  output_schema = NULL,
  emit_events = FALSE
) {
  codex_ensure_thread(provider, tools = tools)
  runtime <- provider@runtime
  start <- Sys.time()

  params <- list(
    threadId = runtime$thread_id,
    input = input
  )
  if (!is.null(output_schema)) {
    params$outputSchema <- output_schema
  }

  request_id <- codex_send_request(provider, "turn/start", params)

  iter <- coro::generator(function() {
    response_received <- FALSE
    deltas <- character()
    final_text <- NULL

    repeat {
      msg <- codex_read_message(provider)

      if (!is.null(msg$id) && as.numeric(msg$id) == as.numeric(request_id)) {
        if (!is.null(msg$error)) {
          cli::cli_abort(msg$error$message %||% "Codex request failed.")
        }
        response_received <- TRUE
        next
      }

      if (!is.null(msg$id) && !is.null(msg$method)) {
        codex_maybe_emit_event(provider, msg, emit_events = emit_events)
        codex_handle_server_request(
          provider,
          msg,
          tools = tools,
          emit_tools = emit_events
        )
        next
      }

      if (is.null(msg$method)) {
        next
      }

      codex_track_command_output(provider, msg)
      codex_maybe_emit_event(provider, msg, emit_events = emit_events)

      if (identical(msg$method, "item/agentMessage/delta")) {
        delta <- msg$params$delta %||% ""
        deltas <- c(deltas, delta)
        yield(list(type = "item/agentMessage/delta", delta = delta))
        next
      }

      if (identical(msg$method, "item/completed")) {
        item <- msg$params$item
        if (identical(item$type, "agentMessage")) {
          final_text <- item$text
        }
        next
      }

      if (identical(msg$method, "turn/completed")) {
        status <- msg$params$turn$status %||% "failed"
        if (!identical(status, "completed")) {
          err <- msg$params$turn$error$message %||% "Codex turn failed."
          cli::cli_abort(err, class = "ellmer_codex_turn_failed")
        }

        if (!response_received) {
          cli::cli_abort("Codex turn did not return a response.")
        }

        if (is.null(final_text)) {
          final_text <- paste0(deltas, collapse = "")
        }
        yield(list(
          type = "turn/completed",
          result = list(
            text = final_text,
            deltas = deltas,
            duration = as.numeric(difftime(Sys.time(), start, units = "secs"))
          )
        ))
        break
      }
    }

    coro::exhausted()
  })

  iter()
}

codex_run_turn <- function(
  provider,
  input,
  tools = NULL,
  output_schema = NULL
) {
  codex_ensure_thread(provider, tools = tools)
  runtime <- provider@runtime
  start <- Sys.time()

  params <- list(
    threadId = runtime$thread_id,
    input = input
  )
  if (!is.null(output_schema)) {
    params$outputSchema <- output_schema
  }

  request_id <- codex_send_request(provider, "turn/start", params)

  response_received <- FALSE
  deltas <- character()
  final_text <- NULL

  repeat {
    msg <- codex_read_message(provider)

    if (!is.null(msg$id) && as.numeric(msg$id) == as.numeric(request_id)) {
      response_received <- TRUE
      next
    }

    if (!is.null(msg$id) && !is.null(msg$method)) {
      codex_handle_server_request(provider, msg, tools = tools)
      next
    }

    if (is.null(msg$method)) {
      next
    }

    codex_track_command_output(provider, msg)

    if (identical(msg$method, "item/agentMessage/delta")) {
      delta <- msg$params$delta %||% ""
      deltas <- c(deltas, delta)
      next
    }

    if (identical(msg$method, "item/completed")) {
      item <- msg$params$item
      if (identical(item$type, "agentMessage")) {
        final_text <- item$text
      }
      next
    }

    if (identical(msg$method, "turn/completed")) {
      status <- msg$params$turn$status %||% "failed"
      if (!identical(status, "completed")) {
        err <- msg$params$turn$error$message %||% "Codex turn failed."
        cli::cli_abort(err, class = "ellmer_codex_turn_failed")
      }
      break
    }
  }

  if (!response_received) {
    cli::cli_abort("Codex turn did not return a response.")
  }

  if (is.null(final_text)) {
    final_text <- paste0(deltas, collapse = "")
  }

  list(
    text = final_text,
    deltas = deltas,
    duration = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
}

codex_ensure_thread <- function(provider, tools = NULL) {
  codex_ensure_initialized(provider)
  runtime <- provider@runtime
  if (!is.null(runtime$thread_id)) {
    codex_assert_tools_locked(provider, tools = tools)
    return(invisible())
  }

  start_params <- list(model = provider@model)
  config <- codex_prepare_thread_config(provider@config)
  if (!is.null(config)) {
    start_params$config <- config
  }
  dynamic_tools <- codex_dynamic_tools(provider, tools)
  if (length(dynamic_tools) > 0) {
    start_params$dynamicTools <- dynamic_tools
  }

  request_id <- codex_send_request(provider, "thread/start", start_params)
  response <- codex_wait_response(provider, request_id)
  runtime$thread_id <- response$result$thread$id
  runtime$tools_signature <- codex_tools_signature(provider, tools = tools)
  invisible()
}

codex_prepare_thread_config <- function(config) {
  if (is.null(config)) {
    return(NULL)
  }
  if (!codex_shell_tool_disabled(config)) {
    return(config)
  }
  codex_append_developer_instructions(config, paste(
    "Do not call list_mcp_resources, list_mcp_resource_templates,",
    "or read_mcp_resource in this thread."
  ))
}

codex_shell_tool_disabled <- function(config) {
  is.list(config) &&
    is.list(config$features) &&
    isFALSE(config$features$shell_tool)
}

codex_append_developer_instructions <- function(config, text) {
  existing <- config$developer_instructions
  if (!is.null(existing) && nzchar(existing)) {
    config$developer_instructions <- paste(existing, text, sep = "\n\n")
  } else {
    config$developer_instructions <- text
  }
  config
}

codex_ensure_initialized <- function(provider) {
  runtime <- provider@runtime
  if (!is.null(runtime$process) && runtime$process$is_alive()) {
    if (isTRUE(runtime$initialized)) {
      return(invisible())
    }
  } else {
    codex_bootstrap_auth(codex_home_dir())
    codex_bin <- codex_resolve_bin(provider@codex_bin)
    runtime$process <- processx::process$new(
      command = codex_bin,
      args = c("app-server"),
      env = c(
        CODEX_HOME = codex_home_dir(),
        PATH = Sys.getenv("PATH", unset = "")
      ),
      stdin = "|",
      stdout = "|",
      stderr = "|",
      cleanup = FALSE
    )
    runtime$codex_home <- codex_home_dir()
    runtime$initialized <- FALSE
    runtime$thread_id <- NULL
    runtime$output_buffer <- ""
    runtime$output_lines <- character()
    runtime$stderr_lines <- character()
    runtime$command_output <- list()
    runtime$status_tool_requests <- list()
    runtime$tools_signature <- NULL
    runtime$next_request_id <- 1
  }

  request_id <- codex_send_request(
    provider,
    "initialize",
    list(
      clientInfo = list(
        name = "r_ellmer",
        title = "ellmer",
        version = as.character(utils::packageVersion("ellmer"))
      ),
      capabilities = list(experimentalApi = TRUE)
    )
  )
  codex_wait_response(provider, request_id)
  codex_send_notification(provider, "initialized", list())
  runtime$initialized <- TRUE
  invisible()
}

codex_resolve_bin <- function(codex_bin) {
  if (grepl("[/\\\\]", codex_bin) || startsWith(codex_bin, "~")) {
    path <- path.expand(codex_bin)
    if (file.exists(path)) {
      return(path)
    }
  } else {
    path <- Sys.which(codex_bin)
    if (nzchar(path)) {
      return(path)
    }
  }

  cli::cli_abort(
    c(
      "Could not find Codex CLI binary {.file {codex_bin}}.",
      "i" = "Install Codex CLI and ensure {.code Sys.which('codex')} returns a path.",
      "i" = "Or pass an explicit binary path with {.code chat_codex(codex_bin = '/full/path/to/codex')}."
    ),
    class = "ellmer_codex_binary_not_found"
  )
}

codex_send_notification <- function(provider, method, params = list()) {
  codex_write_message(provider, list(method = method, params = params))
}

codex_send_request <- function(provider, method, params = list()) {
  runtime <- provider@runtime
  request_id <- runtime$next_request_id
  runtime$next_request_id <- request_id + 1
  codex_write_message(
    provider,
    list(
      method = method,
      id = request_id,
      params = params
    )
  )
  request_id
}

codex_wait_response <- function(provider, request_id, tools = NULL) {
  repeat {
    msg <- codex_read_message(provider)

    if (!is.null(msg$id) && !is.null(msg$method)) {
      codex_handle_server_request(provider, msg, tools = tools)
      next
    }

    if (!is.null(msg$id) && as.numeric(msg$id) == as.numeric(request_id)) {
      if (!is.null(msg$error)) {
        cli::cli_abort(msg$error$message %||% "Codex request failed.")
      }
      return(msg)
    }
  }
}

codex_handle_server_request <- function(
  provider,
  msg,
  tools = NULL,
  emit_tools = FALSE
) {
  method <- msg$method %||% ""
  if (identical(method, "item/commandExecution/requestApproval")) {
    codex_write_message(
      provider,
      list(
        id = msg$id,
        result = list(decision = "decline")
      )
    )
  } else if (identical(method, "item/fileChange/requestApproval")) {
    codex_write_message(
      provider,
      list(
        id = msg$id,
        result = list(decision = "decline")
      )
    )
  } else if (identical(method, "item/tool/call")) {
    result <- codex_tool_call(
      provider,
      msg$params,
      tools = tools,
      emit_tools = emit_tools
    )
    codex_write_message(
      provider,
      list(
        id = msg$id,
        result = result
      )
    )
  } else {
    codex_write_message(provider, list(id = msg$id, result = list()))
  }
}

codex_write_message <- function(provider, msg) {
  json <- unclass(jsonlite::toJSON(msg, auto_unbox = TRUE, null = "null"))
  provider@runtime$process$write_input(paste0(json, "\n"), sep = "")
}

codex_read_message <- function(provider, timeout_ms = 10000) {
  runtime <- provider@runtime
  proc <- runtime$process
  deadline <- Sys.time() + timeout_ms / 1000

  while (Sys.time() < deadline) {
    if (length(runtime$output_lines) > 0) {
      line <- runtime$output_lines[[1]]
      runtime$output_lines <- runtime$output_lines[-1]
      line <- sub("\r$", "", line)
      if (!nzchar(line)) {
        next
      }
      return(jsonlite::parse_json(line, simplifyVector = FALSE))
    }

    io <- proc$poll_io(100)

    if (identical(io[["error"]], "ready")) {
      errors <- proc$read_error_lines()
      if (length(errors) > 0) {
        runtime$stderr_lines <- c(runtime$stderr_lines, errors)
        if (identical(provider@events, "raw")) {
          for (line in errors) {
            message(paste0("[codex stderr] ", line))
          }
        }
      }
    }

    if (identical(io[["output"]], "ready")) {
      lines <- proc$read_output_lines()
      if (length(lines) > 0) {
        runtime$output_lines <- c(runtime$output_lines, lines)
      }
    }

    if (!proc$is_alive()) {
      details <- ""
      tail_lines <- tail(runtime$stderr_lines, 3)
      if (length(tail_lines) > 0) {
        details <- paste0("\n", paste(tail_lines, collapse = "\n"))
      }
      cli::cli_abort(paste0("Codex app-server process exited unexpectedly.", details))
    }
  }

  cli::cli_abort("Timed out waiting for Codex app-server output.")
}

codex_runtime_new <- function() {
  runtime <- new.env(parent = emptyenv())
  runtime$id <- paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    as.integer(stats::runif(1, 1, 1e9))
  )
  runtime$process <- NULL
  runtime$initialized <- FALSE
  runtime$thread_id <- NULL
  runtime$output_buffer <- ""
  runtime$output_lines <- character()
  runtime$stderr_lines <- character()
  runtime$command_output <- list()
  runtime$status_tool_requests <- list()
  runtime$codex_home <- codex_home_dir()
  runtime$tools_signature <- NULL
  runtime$next_request_id <- 1
  codex_register_runtime(runtime)
  reg.finalizer(
    runtime,
    function(x) {
      codex_runtime_stop(x)
      codex_unregister_runtime(x)
    },
    onexit = TRUE
  )
  runtime
}

codex_register_runtime <- function(runtime) {
  .codex_runtime_registry$runtimes[[runtime$id]] <- runtime
  invisible()
}

codex_unregister_runtime <- function(runtime) {
  id <- runtime$id %||% NULL
  if (
    !is.null(id) &&
      exists(id, envir = .codex_runtime_registry$runtimes, inherits = FALSE)
  ) {
    rm(list = id, envir = .codex_runtime_registry$runtimes)
  }
  invisible()
}

codex_runtime_stop <- function(runtime) {
  proc <- runtime$process %||% NULL
  if (is.null(proc)) {
    return(invisible())
  }
  if (proc$is_alive()) {
    try(proc$kill(), silent = TRUE)
  }
  runtime$initialized <- FALSE
  runtime$thread_id <- NULL
  runtime$output_buffer <- ""
  runtime$output_lines <- character()
  runtime$stderr_lines <- character()
  runtime$command_output <- list()
  runtime$status_tool_requests <- list()
  runtime$tools_signature <- NULL
  invisible()
}

codex_home_dir <- function() {
  path <- file.path(tools::R_user_dir("ellmer", which = "data"), "codex")
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  path
}

codex_default_home_dir <- function() {
  env_home <- Sys.getenv("CODEX_HOME", unset = "")
  if (nzchar(env_home)) {
    return(path.expand(env_home))
  }
  path.expand("~/.codex")
}

codex_bootstrap_auth <- function(target_home) {
  target_auth <- file.path(target_home, "auth.json")
  if (file.exists(target_auth)) {
    return(invisible())
  }

  source_home <- codex_default_home_dir()
  source_auth <- file.path(source_home, "auth.json")
  if (
    !file.exists(source_auth) ||
      identical(normalizePath(source_home, winslash = "/", mustWork = FALSE),
                normalizePath(target_home, winslash = "/", mustWork = FALSE))
  ) {
    return(invisible())
  }

  dir.create(target_home, recursive = TRUE, showWarnings = FALSE)
  ok <- file.copy(source_auth, target_auth, overwrite = FALSE)
  if (isTRUE(ok)) {
    Sys.chmod(target_auth, mode = "600")
  }
  invisible()
}

codex_maybe_emit_event <- function(provider, msg, emit_events = FALSE) {
  if (!emit_events || identical(provider@events, "none")) {
    return(invisible())
  }

  if (
    identical(provider@events, "status") &&
      isTRUE(codex_maybe_emit_tool_status(provider, msg))
  ) {
    return(invisible())
  }

  line <- codex_event_line(provider, msg)
  if (!is.null(line)) {
    codex_emit_status_line(line)
  }
  invisible()
}

codex_emit_status_line <- function(line) {
  flush.console()
  message(line)
  try(flush(stderr()), silent = TRUE)
  flush.console()
  invisible()
}

codex_event_line <- function(provider, msg) {
  method <- msg$method %||% NULL
  if (is.null(method) || identical(method, "item/agentMessage/delta")) {
    return(NULL)
  }

  if (identical(provider@events, "raw")) {
    params <- msg$params %||% list()
    payload <- unclass(jsonlite::toJSON(params, auto_unbox = TRUE, null = "null"))
    return(paste0("[codex] ", method, " ", payload))
  }

  if (identical(method, "turn/completed")) {
    status <- msg$params$turn$status %||% "unknown"
    if (identical(status, "completed")) {
      return(NULL)
    }
    return(paste0("[codex] turn ", status))
  }
  if (identical(method, "item/commandExecution/requestApproval")) {
    return("[codex] approval requested: command")
  }
  if (identical(method, "item/fileChange/requestApproval")) {
    return("[codex] approval requested: file changes")
  }
  if (identical(method, "item/started")) {
    item <- msg$params$item %||% list()
    type <- item$type %||% "item"
    if (type %in% c("commandExecution", "fileChange", "mcpToolCall")) {
      return(NULL)
    }
    return(NULL)
  }
  if (identical(method, "item/completed")) {
    item <- msg$params$item %||% list()
    type <- item$type %||% "item"
    if (type %in% c("commandExecution", "fileChange", "mcpToolCall")) {
      if (identical(type, "commandExecution")) {
        codex_clear_command_output(provider, item$id %||% "")
      }
      return(NULL)
    }
    return(NULL)
  }

  NULL
}

codex_command_summary <- function(item) {
  cmd <- item$command %||% character()
  if (length(cmd) == 0) {
    return("<command>")
  }

  if (length(cmd) == 1) {
    summary <- trimws(as.character(cmd[[1]]))
    shell_prefix <- "^(.*/)?(sh|bash|zsh)\\s+-lc\\s+"
    if (grepl(shell_prefix, summary, perl = TRUE)) {
      summary <- sub(shell_prefix, "", summary, perl = TRUE)
      summary <- sub("^(['\"])(.*)\\1$", "\\2", summary, perl = TRUE)
      summary <- gsub("\\\\\"", "\"", summary)
      summary <- gsub("\\\\'", "'", summary)
    }
  } else {
    head <- basename(cmd[[1]])
    if (
      length(cmd) >= 3 &&
        identical(cmd[[2]], "-lc") &&
        head %in% c("sh", "bash", "zsh")
    ) {
      summary <- cmd[[3]]
    } else {
      summary <- paste(cmd, collapse = " ")
    }
  }

  summary <- trimws(gsub("[[:space:]]+", " ", summary))
  if (nchar(summary) > 90) {
    paste0(substr(summary, 1, 87), "...")
  } else {
    summary
  }
}

codex_maybe_emit_tool_status <- function(provider, msg) {
  method <- msg$method %||% ""
  if (!method %in% c("item/started", "item/completed")) {
    return(FALSE)
  }

  item <- msg$params$item %||% list()
  item_id <- item$id %||% ""
  type <- item$type %||% ""
  if (!type %in% c("commandExecution", "fileChange", "mcpToolCall")) {
    return(FALSE)
  }

  if (identical(method, "item/started")) {
    request <- codex_status_tool_request(item)
    provider@runtime$status_tool_requests[[item_id]] <- request
    maybe_echo_tool(request, echo = "output")
    return(TRUE)
  }

  status <- item$status %||% "completed"
  request <- provider@runtime$status_tool_requests[[item_id]] %||%
    codex_status_tool_request(item)
  provider@runtime$status_tool_requests[[item_id]] <- NULL

  if (identical(status, "completed")) {
    return(TRUE)
  }

  err <- codex_status_tool_error(provider, item)
  maybe_echo_tool(
    ContentToolResult(error = err, request = request),
    echo = "output"
  )
  TRUE
}

codex_status_tool_request <- function(item) {
  type <- item$type %||% ""

  if (identical(type, "commandExecution")) {
    return(ContentToolRequest(
      id = item$id %||% "",
      name = "shell",
      arguments = list(command = codex_command_summary(item)),
      tool = NULL,
      extra = list(source = "codex_builtin")
    ))
  }
  if (identical(type, "fileChange")) {
    return(ContentToolRequest(
      id = item$id %||% "",
      name = "apply_patch",
      arguments = list(),
      tool = NULL,
      extra = list(source = "codex_builtin")
    ))
  }

  ContentToolRequest(
    id = item$id %||% "",
    name = item$tool %||% "app_tool",
    arguments = item$arguments %||% list(),
    tool = NULL,
    extra = list(source = "codex_builtin")
  )
}

codex_status_tool_error <- function(provider, item) {
  type <- item$type %||% ""
  status <- item$status %||% "failed"

  if (identical(type, "commandExecution")) {
    reason <- codex_command_failure_reason(provider, item)
    codex_clear_command_output(provider, item$id %||% "")
    return(reason %||% paste("command", status))
  }

  item$error %||% paste(type, status)
}

codex_track_command_output <- function(provider, msg) {
  if (!identical(msg$method %||% "", "item/commandExecution/outputDelta")) {
    return(invisible())
  }

  runtime <- provider@runtime
  item_id <- msg$params$itemId %||% msg$params$item$id %||% ""
  if (!nzchar(item_id)) {
    return(invisible())
  }

  delta <- msg$params$delta %||% msg$params$outputDelta %||% ""
  if (!nzchar(delta)) {
    return(invisible())
  }

  existing <- runtime$command_output[[item_id]] %||% ""
  combined <- paste0(existing, delta)
  if (nchar(combined) > 5000) {
    combined <- substr(combined, nchar(combined) - 4999, nchar(combined))
  }
  runtime$command_output[[item_id]] <- combined
  invisible()
}

codex_command_failure_reason <- function(provider, item) {
  text <- item$aggregatedOutput %||%
    provider@runtime$command_output[[item$id %||% ""]] %||%
    ""
  if (!nzchar(text)) {
    return(NULL)
  }

  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  lines <- trimws(lines)
  lines <- lines[nzchar(lines)]
  if (length(lines) == 0) {
    return(NULL)
  }

  reason <- lines[[length(lines)]]
  if (nchar(reason) > 120) {
    reason <- paste0(substr(reason, 1, 117), "...")
  }
  reason
}

codex_clear_command_output <- function(provider, item_id) {
  if (!nzchar(item_id)) {
    return(invisible())
  }

  provider@runtime$command_output[[item_id]] <- NULL
  invisible()
}

codex_shutdown_all_runtimes <- function() {
  ids <- ls(envir = .codex_runtime_registry$runtimes, all.names = TRUE)
  for (id in ids) {
    runtime <- .codex_runtime_registry$runtimes[[id]]
    codex_runtime_stop(runtime)
    codex_unregister_runtime(runtime)
  }
  invisible()
}

codex_dynamic_tools <- function(provider, tools = NULL) {
  if (is.null(tools) || length(tools) == 0) {
    return(list())
  }

  specs <- lapply(tools, function(tool) {
    if (!S7_inherits(tool, ToolDef)) {
      return(NULL)
    }
    list(
      name = tool@name,
      description = tool@description,
      inputSchema = as_json(provider, tool@arguments)
    )
  })

  unname(specs[!map_lgl(specs, is.null)])
}

codex_tools_signature <- function(provider, tools = NULL) {
  specs <- codex_dynamic_tools(provider, tools = tools)
  unclass(jsonlite::toJSON(specs, auto_unbox = TRUE, null = "null"))
}

codex_assert_tools_locked <- function(provider, tools = NULL) {
  runtime <- provider@runtime
  current <- codex_tools_signature(provider, tools = tools)

  if (is.null(runtime$tools_signature)) {
    runtime$tools_signature <- current
    return(invisible())
  }
  if (!identical(current, runtime$tools_signature)) {
    cli::cli_abort(
      paste0(
        "{.fn chat_codex} does not support changing tools after thread ",
        "start. Create a new chat for a different tool set."
      ),
      class = "ellmer_codex_tool_set_locked"
    )
  }

  invisible()
}

codex_tool_call <- function(
  provider,
  params,
  tools = NULL,
  emit_tools = FALSE
) {
  tool_name <- params$tool %||% ""
  tool <- tools[[tool_name]]
  request <- ContentToolRequest(
    id = params$callId %||% "",
    name = tool_name,
    arguments = params$arguments %||% list(),
    tool = tool
  )

  if (emit_tools) {
    maybe_echo_tool(request, echo = "output")
  }

  result <- invoke_tool(request)
  if (emit_tools) {
    maybe_echo_tool(result, echo = "output")
  }

  if (tool_errored(result)) {
    return(list(
      contentItems = list(list(
        type = "inputText",
        text = tool_error_string(result)
      )),
      success = FALSE
    ))
  }

  value <- result@value
  if (S7_inherits(value, ContentImageRemote)) {
    return(list(
      contentItems = list(list(type = "inputImage", imageUrl = value@url)),
      success = TRUE
    ))
  }
  if (S7_inherits(value, ContentImageInline)) {
    return(list(
      contentItems = list(list(
        type = "inputImage",
        imageUrl = paste0("data:", value@type, ";base64,", value@data)
      )),
      success = TRUE
    ))
  }
  if (S7_inherits(value, Content)) {
    return(list(
      contentItems = list(list(
        type = "inputText",
        text = paste0(
          "Unsupported tool output type for chat_codex: ",
          class_name(value)
        )
      )),
      success = FALSE
    ))
  }

  list(
    contentItems = list(list(type = "inputText", text = tool_string(result))),
    success = TRUE
  )
}
