test_that("chat_codex() creates a chat with a codex provider", {
  chat <- chat_codex(echo = "none")
  provider <- chat$get_provider()

  expect_true(S7_inherits(provider, ProviderCodex))
  expect_equal(provider@name, "Codex")
  expect_equal(provider@model, "gpt-5.3-codex")
  expect_equal(provider@codex_bin, "codex")
  expect_null(provider@config)
})

test_that("chat_codex() validates config argument type", {
  expect_error(
    chat_codex(config = "not-a-list"),
    "config.*must be a list or NULL"
  )
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

mock_codex_bin_with_command_event <- function() {
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
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_evt', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "    elif method == 'turn/start':",
    "        turn = {'id': 'turn_evt', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        send({'method': 'item/started', 'params': {'threadId': 'thr_evt', 'turnId': 'turn_evt', 'item': {'type': 'commandExecution', 'id': 'cmd_1', 'status': 'inProgress', 'command': ['/bin/zsh', '-lc', 'pwd']}}})",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_evt', 'turnId': 'turn_evt', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': 'hello from mock'}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_evt', 'turn': {'id': 'turn_evt', 'status': 'completed', 'items': [], 'error': None}}})",
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

mock_codex_bin_echo_path <- function() {
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
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_path', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "    elif method == 'turn/start':",
    "        turn = {'id': 'turn_path', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        text = os.environ.get('PATH', '')",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_path', 'turnId': 'turn_path', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': text}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_path', 'turn': {'id': 'turn_path', 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

mock_codex_bin_echo_config <- function() {
  path <- tempfile(fileext = ".py")
  code <- c(
    "#!/usr/bin/env python3",
    "import json, sys",
    "",
    "thread_config = None",
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
    "        thread_config = (msg.get('params') or {}).get('config')",
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_cfg', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "    elif method == 'turn/start':",
    "        turn = {'id': 'turn_cfg', 'status': 'inProgress', 'items': [], 'error': None}",
    "        send({'id': req_id, 'result': {'turn': turn}})",
    "        text = json.dumps(thread_config or {}, sort_keys=True)",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_cfg', 'turnId': 'turn_cfg', 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': text}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_cfg', 'turn': {'id': 'turn_cfg', 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

test_that("chat_codex() passes PATH through to app-server process", {
  codex_bin <- mock_codex_bin_echo_path()
  chat <- chat_codex(codex_bin = codex_bin, echo = "none")
  out <- as.character(chat$chat("Report PATH"))

  expect_equal(out, Sys.getenv("PATH", unset = ""))
})

test_that("chat_codex() forwards config overrides to thread/start", {
  codex_bin <- mock_codex_bin_echo_config()
  cfg <- list(
    features = list(shell_tool = FALSE),
    tools = list(view_image = FALSE)
  )
  chat <- chat_codex(codex_bin = codex_bin, config = cfg, echo = "none")
  out <- as.character(chat$chat("Report config"))
  parsed <- jsonlite::parse_json(out, simplifyVector = TRUE)

  expect_false(isTRUE(parsed$features$shell_tool))
  expect_false(isTRUE(parsed$tools$view_image))
  expect_match(
    parsed$developer_instructions,
    "Do not call list_mcp_resources",
    fixed = TRUE
  )
})

test_that("codex_prepare_thread_config() adds mcp suppression when shell tool is disabled", {
  cfg <- list(features = list(shell_tool = FALSE))
  prepared <- codex_prepare_thread_config(cfg)

  expect_false(isTRUE(prepared$features$shell_tool))
  expect_match(
    prepared$developer_instructions,
    "Do not call list_mcp_resources",
    fixed = TRUE
  )
})

test_that("codex_prepare_thread_config() appends to existing developer instructions", {
  cfg <- list(
    features = list(shell_tool = FALSE),
    developer_instructions = "Stay concise."
  )
  prepared <- codex_prepare_thread_config(cfg)

  expect_match(prepared$developer_instructions, "Stay concise.", fixed = TRUE)
  expect_match(
    prepared$developer_instructions,
    "Do not call list_mcp_resources",
    fixed = TRUE
  )
})

test_that("codex_prepare_thread_config() is unchanged when shell tool is enabled", {
  cfg <- list(features = list(shell_tool = TRUE))
  prepared <- codex_prepare_thread_config(cfg)

  expect_false("developer_instructions" %in% names(prepared))
})

test_that("chat_codex() emits app-server status events in stream mode", {
  codex_bin <- mock_codex_bin_with_command_event()
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
  ), type = "output")
  output <- capture.output({
    result <- chat$chat("Emit events again")
  }, type = "output")

  expect_equal(as.character(result), "hello from mock")
  expect_true(any(grepl("\\[tool call\\]", messages)))
  expect_true(any(grepl("shell\\(", messages)))
  expect_false(any(grepl("\\[tool call\\]", output)))
})

test_that("codex status event formatting is concise by default", {
  provider <- chat_codex(echo = "none")$get_provider()

  expect_null(codex_event_line(provider, list(
    method = "turn/started",
    params = list(turn = list(status = "inProgress"))
  )))
  expect_null(codex_event_line(provider, list(
    method = "item/started",
    params = list(item = list(type = "reasoning"))
  )))
  expect_null(codex_event_line(provider, list(
    method = "item/completed",
    params = list(item = list(type = "agentMessage"))
  )))

  expect_null(
    codex_event_line(provider, list(
      method = "item/started",
      params = list(item = list(
        type = "commandExecution",
        command = list("/bin/zsh", "-lc", "ls -la && echo done")
      ))
    ))
  )
  expect_null(
    codex_event_line(provider, list(
      method = "item/started",
      params = list(item = list(
        type = "commandExecution",
        command = "/bin/zsh -lc \"sed -n '1,220p' DESCRIPTION\""
      ))
    ))
  )
  expect_null(
    codex_event_line(provider, list(
      method = "item/completed",
      params = list(item = list(type = "commandExecution", status = "failed"))
    ))
  )
  expect_null(
    codex_event_line(provider, list(
      method = "item/tool/call",
      params = list(tool = "add_one")
    ))
  )
  expect_null(
    codex_event_line(provider, list(
      method = "turn/completed",
      params = list(turn = list(status = "completed"))
    ))
  )
})

test_that("codex tool-status helpers build request and failure reason", {
  provider <- chat_codex(echo = "none")$get_provider()

  req <- codex_status_tool_request(list(
    type = "commandExecution",
    id = "cmd-1",
    command = "/bin/zsh -lc \"sed -n '1,220p' DESCRIPTION\""
  ))
  expect_s7_class(req, ContentToolRequest)
  expect_equal(req@name, "shell")
  expect_equal(req@arguments$command, "sed -n '1,220p' DESCRIPTION")

  codex_track_command_output(provider, list(
    method = "item/commandExecution/outputDelta",
    params = list(
      itemId = "cmd-1",
      delta = "/bin/zsh:1: command not found: rg"
    )
  ))

  err <- codex_status_tool_error(provider, list(
    type = "commandExecution",
    id = "cmd-1",
    status = "failed"
  ))

  expect_true(grepl("command not found: rg", err))
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

test_that("chat_codex() echoes dynamic tool calls with ellmer tool formatting", {
  codex_bin <- mock_codex_bin_with_tool_call()
  chat <- chat_codex(codex_bin = codex_bin, echo = "output")
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
