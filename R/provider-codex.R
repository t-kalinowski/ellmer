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
    codex_bin = codex_bin
  )

  Chat$new(provider = provider, system_prompt = system_prompt, echo = echo)
}

ProviderCodex <- new_class(
  "ProviderCodex",
  parent = Provider,
  properties = list(
    codex_bin = prop_string()
  )
)
