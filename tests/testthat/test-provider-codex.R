test_that("chat_codex() creates a chat with a codex provider", {
  chat <- chat_codex(echo = "none")
  provider <- chat$get_provider()

  expect_true(S7_inherits(provider, ProviderCodex))
  expect_equal(provider@name, "Codex")
  expect_equal(provider@model, "gpt-5.3-codex")
  expect_equal(provider@codex_bin, "codex")
})

test_that("chat_codex() errors clearly when codex binary is missing", {
  chat <- chat_codex(codex_bin = tempfile("missing-codex-"), echo = "none")

  expect_error(
    chat$chat("hello"),
    class = "ellmer_codex_binary_not_found"
  )
})

test_that("codex bootstrap copies auth.json into isolated codex home", {
  source_home <- tempfile("codex-src-")
  target_home <- tempfile("codex-target-")
  dir.create(source_home, recursive = TRUE)
  dir.create(target_home, recursive = TRUE)
  writeLines('{"auth_mode":"chatgpt"}', file.path(source_home, "auth.json"))

  withr::local_envvar(c(CODEX_HOME = source_home))
  codex_bootstrap_auth(target_home)

  expect_true(file.exists(file.path(target_home, "auth.json")))
  copied <- jsonlite::read_json(file.path(target_home, "auth.json"))
  expect_equal(copied$auth_mode, "chatgpt")
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

test_that("chat_codex() supports stream-mode chat path", {
  codex_bin <- mock_codex_bin()
  chat <- chat_codex(codex_bin = codex_bin, echo = "output")

  out <- chat$chat("Say hello in stream mode")
  expect_equal(as.character(out), "hello from mock")
})

mock_codex_bin_echo_env <- function() {
  path <- tempfile(fileext = ".py")
  code <- c(
    "#!/usr/bin/env python3",
    "import json, os, sys",
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
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_home', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "    elif method == 'turn/start':",
    "        turn = {'id': 'turn_home', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        text = os.environ.get('CODEX_HOME', '')",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_home', 'turnId': 'turn_home', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': text}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_home', 'turn': {'id': 'turn_home', 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

test_that("chat_codex() runs with an ellmer-local CODEX_HOME", {
  codex_bin <- mock_codex_bin_echo_env()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")

  out <- as.character(chat$chat("Report CODEX_HOME"))
  expected <- normalizePath(
    file.path(tools::R_user_dir("ellmer", which = "data"), "codex"),
    winslash = "/",
    mustWork = FALSE
  )
  actual <- normalizePath(out, winslash = "/", mustWork = FALSE)

  expect_equal(actual, expected)
})

test_that("chat_codex() emits app-server status events in stream mode", {
  codex_bin <- mock_codex_bin()
  chat <- chat_codex(codex_bin = codex_bin, echo = "output")

  messages <- character()
  result <- NULL
  capture.output(withCallingHandlers(
    {
      result <- chat$chat("Emit events")
    },
    message = function(cnd) {
      messages <<- c(messages, conditionMessage(cnd))
      invokeRestart("muffleMessage")
    }
  ))

  expect_equal(as.character(result), "hello from mock")
  expect_true(any(grepl("^\\[codex\\]", messages)))
})

mock_codex_bin_with_tool_call <- function() {
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
    "        dynamic_tools = (msg.get('params') or {}).get('dynamicTools') or []",
    "        if not dynamic_tools or dynamic_tools[0].get('name') != 'add_one':",
    "            send({'id': req_id, 'error': {'code': 1, 'message': 'dynamic tools missing'}})",
    "            continue",
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_tools', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "        send({'method': 'thread/started', 'params': {'thread': {'id': 'thr_tools'}}})",
    "    elif method == 'turn/start':",
    "        turn = {'id': 'turn_tools', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        send({'method': 'item/tool/call', 'id': 60, 'params': {'threadId': 'thr_tools', 'turnId': 'turn_tools', 'callId': 'call_1', 'tool': 'add_one', 'arguments': {'x': 2}}})",
    "        tool_result = None",
    "        while tool_result is None:",
    "            incoming = sys.stdin.readline()",
    "            if not incoming:",
    "                sys.exit(1)",
    "            incoming = incoming.strip()",
    "            if not incoming:",
    "                continue",
    "            parsed = json.loads(incoming)",
    "            if parsed.get('id') == 60:",
    "                tool_result = parsed",
    "        text = ((tool_result.get('result') or {}).get('contentItems') or [{}])[0].get('text', '')",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_tools', 'turnId': 'turn_tools', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': text}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_tools', 'turn': {'id': 'turn_tools', 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

test_that("chat_codex() bridges dynamic tool calls through ellmer tools", {
  codex_bin <- mock_codex_bin_with_tool_call()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")
  chat$register_tool(tool(
    function(x) x + 1,
    name = "add_one",
    description = "Add one",
    arguments = list(x = type_integer("Input integer"))
  ))

  out <- chat$chat("Use the tool")
  expect_equal(as.character(out), "3")
})

mock_codex_bin_structured <- function() {
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
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_struct', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "    elif method == 'turn/start':",
    "        schema = (msg.get('params') or {}).get('outputSchema') or {}",
    "        if schema.get('type') != 'object':",
    "            send({'id': req_id, 'error': {'code': 1, 'message': 'missing outputSchema'}})",
    "            continue",
    "        turn = {'id': 'turn_struct', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_struct', 'turnId': 'turn_struct', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': '{\"answer\":\"ok\"}'}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_struct', 'turn': {'id': 'turn_struct', 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

test_that("chat_codex() supports structured output via outputSchema", {
  codex_bin <- mock_codex_bin_structured()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")

  out <- chat$chat_structured(
    "Return structured output",
    type = type_object(answer = type_string("Final answer"))
  )

  expect_equal(out$answer, "ok")
})

test_that("chat_codex() locks tool set after first thread start", {
  codex_bin <- mock_codex_bin()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")
  chat$register_tool(tool(
    function(x) x + 1,
    name = "add_one",
    description = "Add one",
    arguments = list(x = type_integer("Input integer"))
  ))

  chat$chat("First turn")

  chat$register_tool(tool(
    function(x) x * 2,
    name = "times_two",
    description = "Multiply by two",
    arguments = list(x = type_integer("Input integer"))
  ))

  expect_error(
    chat$chat("Second turn"),
    class = "ellmer_codex_tool_set_locked"
  )
})

test_that("chat_codex() fails fast for unsupported async, batch, and parallel APIs", {
  codex_bin <- mock_codex_bin()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")

  expect_error(
    chat$chat_async("hi"),
    class = "ellmer_codex_async_not_supported"
  )
  expect_error(
    chat$stream_async("hi"),
    class = "ellmer_codex_async_not_supported"
  )
  expect_error(
    chat$chat_structured_async("hi", type = type_object(x = type_string())),
    class = "ellmer_codex_async_not_supported"
  )

  prompts <- list("a", "b")
  expect_error(
    batch_chat(chat, prompts, path = tempfile(fileext = ".json"), wait = FALSE),
    class = "ellmer_codex_batch_not_supported"
  )
  expect_error(
    batch_chat_structured(
      chat,
      prompts,
      path = tempfile(fileext = ".json"),
      type = type_object(x = type_string()),
      wait = FALSE
    ),
    class = "ellmer_codex_batch_not_supported"
  )

  expect_error(
    parallel_chat(chat, prompts),
    class = "ellmer_codex_parallel_not_supported"
  )
  expect_error(
    parallel_chat_structured(chat, prompts, type = type_object(x = type_string())),
    class = "ellmer_codex_parallel_not_supported"
  )
})

test_that("codex runtime cleanup stops child processes", {
  runtime <- codex_runtime_new()
  runtime$process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("-e", "Sys.sleep(30)"),
    stdin = "|",
    stdout = "|",
    stderr = "|",
    cleanup = FALSE
  )
  expect_true(runtime$process$is_alive())

  codex_runtime_stop(runtime)
  Sys.sleep(0.1)
  expect_false(runtime$process$is_alive())
})

test_that("global codex cleanup stops all tracked runtimes", {
  runtime1 <- codex_runtime_new()
  runtime2 <- codex_runtime_new()
  runtime1$process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("-e", "Sys.sleep(30)"),
    stdin = "|",
    stdout = "|",
    stderr = "|",
    cleanup = FALSE
  )
  runtime2$process <- processx::process$new(
    command = file.path(R.home("bin"), "Rscript"),
    args = c("-e", "Sys.sleep(30)"),
    stdin = "|",
    stdout = "|",
    stderr = "|",
    cleanup = FALSE
  )
  expect_true(runtime1$process$is_alive())
  expect_true(runtime2$process$is_alive())

  codex_shutdown_all_runtimes()
  Sys.sleep(0.1)
  expect_false(runtime1$process$is_alive())
  expect_false(runtime2$process$is_alive())
})
