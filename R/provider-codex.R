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
  model = "gpt-5-codex",
  params = NULL,
  codex_bin = "codex",
  echo = c("none", "output", "all")
) {
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
    runtime = codex_runtime_new()
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderCodex <- new_class(
  "ProviderCodex",
  parent = Provider,
  properties = list(
    codex_bin = prop_string(),
    runtime = class_any
  )
)

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
  result <- codex_run_turn(
    provider,
    input,
    tools = tools,
    output_schema = output_schema
  )

  if (mode == "value") {
    result
  } else {
    coro::generator(function() {
      for (delta in result$deltas) {
        yield(list(type = "item/agentMessage/delta", delta = delta))
      }
      yield(list(type = "turn/completed", result = result))
      coro::exhausted()
    })
  }
}

method(chat_response_body, ProviderCodex) <- function(provider, response) {
  response
}

method(chat_response_duration, ProviderCodex) <- function(provider, response) {
  response$duration
}

method(stream_content, ProviderCodex) <- function(provider, event) {
  if (is.null(event) || !identical(event$type, "item/agentMessage/delta")) {
    return(NULL)
  }
  ContentText(event$delta)
}

method(stream_merge_chunks, ProviderCodex) <- function(provider, result, chunk) {
  if (is.null(chunk) || !identical(chunk$type, "turn/completed")) {
    return(result)
  }
  chunk$result
}

method(value_turn, ProviderCodex) <- function(provider, result, has_type = FALSE) {
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

codex_ensure_initialized <- function(provider) {
  runtime <- provider@runtime
  if (!is.null(runtime$process) && runtime$process$is_alive()) {
    if (isTRUE(runtime$initialized)) {
      return(invisible())
    }
  } else {
    runtime$process <- processx::process$new(
      command = provider@codex_bin,
      args = c("app-server"),
      stdin = "|",
      stdout = "|",
      stderr = "|",
      cleanup = FALSE
    )
    runtime$initialized <- FALSE
    runtime$thread_id <- NULL
    runtime$output_buffer <- ""
    runtime$output_lines <- character()
    runtime$tools_signature <- NULL
    runtime$next_request_id <- 1
  }

  request_id <- codex_send_request(provider, "initialize", list(
    clientInfo = list(
      name = "r_ellmer",
      title = "ellmer",
      version = as.character(utils::packageVersion("ellmer"))
    ),
    capabilities = list(experimentalApi = TRUE)
  ))
  codex_wait_response(provider, request_id)
  codex_send_notification(provider, "initialized", list())
  runtime$initialized <- TRUE
  invisible()
}

codex_send_notification <- function(provider, method, params = list()) {
  codex_write_message(provider, list(method = method, params = params))
}

codex_send_request <- function(provider, method, params = list()) {
  runtime <- provider@runtime
  request_id <- runtime$next_request_id
  runtime$next_request_id <- request_id + 1
  codex_write_message(provider, list(
    method = method,
    id = request_id,
    params = params
  ))
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

codex_handle_server_request <- function(provider, msg, tools = NULL) {
  method <- msg$method %||% ""
  if (identical(method, "item/commandExecution/requestApproval")) {
    codex_write_message(provider, list(
      id = msg$id,
      result = list(decision = "decline")
    ))
  } else if (identical(method, "item/fileChange/requestApproval")) {
    codex_write_message(provider, list(
      id = msg$id,
      result = list(decision = "decline")
    ))
  } else if (identical(method, "item/tool/call")) {
    result <- codex_tool_call(provider, msg$params, tools = tools)
    codex_write_message(provider, list(
      id = msg$id,
      result = result
    ))
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
        cli::cli_warn("codex app-server stderr: {errors[[1]]}")
      }
    }

    if (identical(io[["output"]], "ready")) {
      lines <- proc$read_output_lines()
      if (length(lines) > 0) {
        runtime$output_lines <- c(runtime$output_lines, lines)
      }
    }

    if (!proc$is_alive()) {
      cli::cli_abort("Codex app-server process exited unexpectedly.")
    }
  }

  cli::cli_abort("Timed out waiting for Codex app-server output.")
}

codex_runtime_new <- function() {
  runtime <- new.env(parent = emptyenv())
  runtime$process <- NULL
  runtime$initialized <- FALSE
  runtime$thread_id <- NULL
  runtime$output_buffer <- ""
  runtime$output_lines <- character()
  runtime$tools_signature <- NULL
  runtime$next_request_id <- 1
  runtime
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

codex_tool_call <- function(provider, params, tools = NULL) {
  tool_name <- params$tool %||% ""
  tool <- tools[[tool_name]]
  request <- ContentToolRequest(
    id = params$callId %||% "",
    name = tool_name,
    arguments = params$arguments %||% list(),
    tool = tool
  )

  result <- invoke_tool(request)
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
