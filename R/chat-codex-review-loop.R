#' Run a Codex built-in review/fix loop
#'
#' @description
#' Uses Codex app-server's built-in `review/start` workflow, then asks Codex to
#' fix findings, and repeats until the review is clean or `max_iterations` is
#' reached.
#'
#' @param chat A [Chat] object created by [chat_codex()].
#' @param max_iterations Maximum number of review iterations.
#' @param target Review target: `"uncommitted_changes"` (default),
#'   `"base_branch"`, `"commit"`, or `"custom"`.
#' @param base_branch Base branch name when `target = "base_branch"`.
#' @param commit_sha Commit SHA when `target = "commit"`.
#' @param commit_title Optional title when `target = "commit"`.
#' @param custom_instructions Instructions when `target = "custom"`.
#' @param delivery Review delivery mode: `"inline"` (default) or `"detached"`.
#' @param fix_prompt Prompt used for the fix turn when review is not clean.
#' @param is_clean Function taking `review_text` and returning a single logical.
#' @param emit_events Whether to emit Codex provider status events while running.
#'
#' @returns A list with `status`, `iterations`, `history`, `thread_id`,
#'   and `review_thread_id`.
#' @export
chat_codex_review_loop <- function(
  chat,
  max_iterations = 5L,
  target = c("uncommitted_changes", "base_branch", "commit", "custom"),
  base_branch = NULL,
  commit_sha = NULL,
  commit_title = NULL,
  custom_instructions = NULL,
  delivery = c("inline", "detached"),
  fix_prompt = paste(
    "Fix all findings from the Codex review.",
    "Keep changes minimal and then summarize what you changed."
  ),
  is_clean = NULL,
  emit_events = FALSE
) {
  if (!inherits(chat, "Chat")) {
    cli::cli_abort("{.arg chat} must be a {.cls Chat} object.")
  }

  provider <- chat$get_provider()
  if (!S7_inherits(provider, ProviderCodex)) {
    cli::cli_abort(
      "{.fn chat_codex_review_loop} requires a chat created by {.fn chat_codex}."
    )
  }

  if (!is.numeric(max_iterations) || length(max_iterations) != 1 || max_iterations < 1) {
    cli::cli_abort("{.arg max_iterations} must be a single number >= 1.")
  }
  max_iterations <- as.integer(max_iterations)

  target <- arg_match(target)
  delivery <- arg_match(delivery)
  target_payload <- codex_review_target_payload(
    target = target,
    base_branch = base_branch,
    commit_sha = commit_sha,
    commit_title = commit_title,
    custom_instructions = custom_instructions
  )

  if (!is.function(is_clean)) {
    is_clean <- codex_default_review_is_clean
  }

  tools <- chat$get_tools()
  codex_ensure_thread(provider, tools = tools)

  history <- vector("list", max_iterations)
  review_thread_id <- NULL

  for (i in seq_len(max_iterations)) {
    review <- codex_run_review(
      provider = provider,
      target = target_payload,
      delivery = delivery,
      tools = tools,
      emit_events = emit_events
    )
    review_thread_id <- review$review_thread_id %||% review_thread_id

    clean <- is_clean(review$text)
    if (!is.logical(clean) || length(clean) != 1 || is.na(clean)) {
      cli::cli_abort("{.arg is_clean} must return a single TRUE/FALSE value.")
    }

    history[[i]] <- list(
      review = review$text,
      fix = NULL,
      clean = clean
    )

    if (isTRUE(clean)) {
      return(list(
        status = "clean",
        iterations = i,
        history = history[seq_len(i)],
        thread_id = provider@runtime$thread_id,
        review_thread_id = review_thread_id
      ))
    }

    fix <- codex_run_turn(
      provider = provider,
      input = list(list(
        type = "text",
        text = paste0(fix_prompt, "\n\nReview findings:\n", review$text)
      )),
      tools = tools,
      output_schema = NULL
    )
    history[[i]]$fix <- fix$text %||% ""
  }

  list(
    status = "max_iterations",
    iterations = max_iterations,
    history = history,
    thread_id = provider@runtime$thread_id,
    review_thread_id = review_thread_id
  )
}

codex_default_review_is_clean <- function(review_text) {
  grepl(
    "(?i)\\b(no issues found|no issues|looks solid|looks good|clean)\\b",
    review_text %||% "",
    perl = TRUE
  )
}

codex_review_target_payload <- function(
  target,
  base_branch = NULL,
  commit_sha = NULL,
  commit_title = NULL,
  custom_instructions = NULL
) {
  switch(
    target,
    uncommitted_changes = list(type = "uncommittedChanges"),
    base_branch = {
      if (!is.character(base_branch) || length(base_branch) != 1 || !nzchar(base_branch)) {
        cli::cli_abort(
          "{.arg base_branch} must be a non-empty string when {.arg target = 'base_branch'}."
        )
      }
      list(type = "baseBranch", branch = base_branch)
    },
    commit = {
      if (!is.character(commit_sha) || length(commit_sha) != 1 || !nzchar(commit_sha)) {
        cli::cli_abort(
          "{.arg commit_sha} must be a non-empty string when {.arg target = 'commit'}."
        )
      }
      list(type = "commit", sha = commit_sha, title = commit_title)
    },
    custom = {
      if (
        !is.character(custom_instructions) ||
          length(custom_instructions) != 1 ||
          !nzchar(custom_instructions)
      ) {
        cli::cli_abort(
          "{.arg custom_instructions} must be a non-empty string when {.arg target = 'custom'}."
        )
      }
      list(type = "custom", instructions = custom_instructions)
    }
  )
}

codex_run_review <- function(
  provider,
  target,
  delivery = c("inline", "detached"),
  tools = NULL,
  emit_events = FALSE
) {
  delivery <- arg_match(delivery)
  codex_ensure_thread(provider, tools = tools)
  runtime <- provider@runtime
  start <- Sys.time()

  request_id <- codex_send_request(
    provider,
    "review/start",
    list(
      threadId = runtime$thread_id,
      delivery = delivery,
      target = target
    )
  )

  response <- NULL
  review_turn_id <- NULL
  review_thread_id <- NULL
  review_text <- NULL
  deltas <- character()

  repeat {
    msg <- codex_read_message(provider)

    if (!is.null(msg$id) && as.numeric(msg$id) == as.numeric(request_id)) {
      if (!is.null(msg$error)) {
        cli::cli_abort(msg$error$message %||% "Codex review request failed.")
      }
      response <- msg
      review_turn_id <- msg$result$turn$id %||% NULL
      review_thread_id <- msg$result$reviewThreadId %||% runtime$thread_id
      if (identical(delivery, "detached") && !is.null(review_thread_id)) {
        runtime$thread_id <- review_thread_id
      }
      next
    }

    if (!is.null(msg$id) && !is.null(msg$method)) {
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
      deltas <- c(deltas, msg$params$delta %||% "")
      next
    }

    if (identical(msg$method, "item/completed")) {
      item <- msg$params$item
      if (identical(item$type, "exitedReviewMode")) {
        review_text <- item$review %||% review_text
      } else if (identical(item$type, "agentMessage") && is.null(review_text)) {
        review_text <- item$text
      }
      next
    }

    if (identical(msg$method, "turn/completed")) {
      turn <- msg$params$turn
      if (!is.null(review_turn_id) && !identical(turn$id %||% "", review_turn_id)) {
        next
      }
      status <- turn$status %||% "failed"
      if (!identical(status, "completed")) {
        err <- turn$error$message %||% "Codex review turn failed."
        cli::cli_abort(err, class = "ellmer_codex_review_failed")
      }
      break
    }
  }

  if (is.null(response)) {
    cli::cli_abort("Codex review did not return a response.")
  }

  if (is.null(review_text)) {
    review_text <- paste0(deltas, collapse = "")
  }

  list(
    text = review_text %||% "",
    turn_id = review_turn_id,
    review_thread_id = review_thread_id,
    duration = as.numeric(difftime(Sys.time(), start, units = "secs"))
  )
}
