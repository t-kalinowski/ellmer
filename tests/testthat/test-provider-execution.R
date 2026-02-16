test_that("provider-specific execution path can bypass httr2 transport", {
  ProviderExecTest <- new_class("ProviderExecTest", parent = Provider)

  provider <- ProviderExecTest(
    name = "exec-test",
    model = "dummy",
    base_url = "unused",
    params = params(),
    extra_args = list(),
    extra_headers = character(),
    credentials = NULL
  )

  method(chat_perform_provider, ProviderExecTest) <- function(
    provider,
    mode = c("value", "stream", "async-stream", "async-value"),
    turns,
    tools = NULL,
    type = NULL
  ) {
    mode <- arg_match(mode)
    if (mode == "value") {
      list(text = "custom execution", duration = 0.123)
    } else {
      cli::cli_abort("Not implemented for test mode {.val {mode}}")
    }
  }

  method(chat_response_body, ProviderExecTest) <- function(provider, response) {
    response
  }

  method(chat_response_duration, ProviderExecTest) <- function(provider, response) {
    response$duration
  }

  method(value_turn, ProviderExecTest) <- function(
    provider,
    result,
    has_type = FALSE
  ) {
    AssistantTurn(
      contents = list(ContentText(result$text)),
      json = list(),
      tokens = unlist(tokens())
    )
  }

  chat <- Chat$new(provider = provider)
  out <- chat$chat("Hello")

  expect_equal(as.character(out), "custom execution")
  expect_equal(chat$last_turn()@duration, 0.123)
})
