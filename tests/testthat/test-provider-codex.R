test_that("chat_codex() creates a chat with a codex provider", {
  chat <- chat_codex(echo = "none")
  provider <- chat$get_provider()

  expect_true(S7_inherits(provider, ProviderCodex))
  expect_equal(provider@name, "Codex")
  expect_equal(provider@model, "gpt-5-codex")
  expect_equal(provider@codex_bin, "codex")
})

mock_codex_bin <- function() {
  path <- tempfile(fileext = ".py")
  code <- c(
    "#!/usr/bin/env python3",
    "import json, sys",
    "",
    "def send(msg):",
    "    sys.stdout.write(json.dumps(msg) + '\\n')",
    "    sys.stdout.flush()",
    "",
    "for line in sys.stdin:",
    "    line = line.strip()",
    "    if not line:",
    "        continue",
    "    msg = json.loads(line)",
    "    method = msg.get('method')",
    "    req_id = msg.get('id')",
    "",
    "    if method == 'initialize':",
    "        send({'id': req_id, 'result': {'userAgent': 'mock-codex/0.1'}})",
    "    elif method == 'initialized':",
    "        continue",
    "    elif method == 'thread/start':",
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_mock', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "        send({'method': 'thread/started', 'params': {'thread': {'id': 'thr_mock'}}})",
    "    elif method == 'turn/start':",
    "        turn = {'id': 'turn_mock', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        send({'method': 'turn/started', 'params': {'threadId': 'thr_mock', 'turn': turn}})",
    "        send({'method': 'item/started', 'params': {'threadId': 'thr_mock', 'turnId': 'turn_mock', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': ''}}})",
    "        send({'method': 'item/agentMessage/delta', 'params': {'threadId': 'thr_mock', 'turnId': 'turn_mock', 'itemId': 'item_1', 'delta': 'hello '}})",
    "        send({'method': 'item/agentMessage/delta', 'params': {'threadId': 'thr_mock', 'turnId': 'turn_mock', 'itemId': 'item_1', 'delta': 'from mock'}})",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_mock', 'turnId': 'turn_mock', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': 'hello from mock'}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_mock', 'turn': {'id': 'turn_mock', 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

test_that("chat_codex() can run a basic turn via app-server protocol", {
  codex_bin <- mock_codex_bin()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")

  out <- chat$chat("Say hello")
  expect_equal(as.character(out), "hello from mock")
})
