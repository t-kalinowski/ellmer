mock_codex_bin_review_loop <- function() {
  path <- tempfile(fileext = ".py")
  code <- c(
    "#!/usr/bin/env python3",
    "import json, sys",
    "",
    "review_count = 0",
    "methods = []",
    "",
    "def send(msg):",
    "    sys.stdout.write(json.dumps(msg) + '\\n')",
    "    sys.stdout.flush()",
    "",
    "def user_text(msg):",
    "    params = msg.get('params') or {}",
    "    items = params.get('input') or []",
    "    parts = []",
    "    for x in items:",
    "        if x.get('type') == 'text':",
    "            parts.append(x.get('text') or '')",
    "    return '\\n'.join(parts)",
    "",
    "for line in sys.stdin:",
    "    line = line.strip()",
    "    if not line:",
    "        continue",
    "    msg = json.loads(line)",
    "    method = msg.get('method')",
    "    req_id = msg.get('id')",
    "    if method is not None:",
    "        methods.append(method)",
    "",
    "    if method == 'initialize':",
    "        send({'id': req_id, 'result': {'userAgent': 'mock-codex/0.1'}})",
    "    elif method == 'initialized':",
    "        continue",
    "    elif method == 'thread/start':",
    "        send({'id': req_id, 'result': {'thread': {'id': 'thr_review', 'preview': '', 'modelProvider': 'openai', 'createdAt': 0}}})",
    "    elif method == 'review/start':",
    "        review_count += 1",
    "        turn_id = 'review_' + str(review_count)",
    "        send({'id': req_id, 'result': {'turn': {'id': turn_id, 'status': 'inProgress', 'items': [], 'error': None}, 'reviewThreadId': 'thr_review'}})",
    "        if review_count == 1:",
    "            review_text = 'Issue found: update docs.'",
    "        else:",
    "            review_text = 'No issues found.'",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_review', 'turnId': turn_id, 'item': {'type': 'exitedReviewMode', 'id': turn_id, 'review': review_text}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_review', 'turn': {'id': turn_id, 'status': 'completed', 'items': [], 'error': None}}})",
    "    elif method == 'turn/start':",
    "        turn_id = 'turn_fix'",
    "        text = user_text(msg)",
    "        send({'id': req_id, 'result': {'turn': {'id': turn_id, 'status': 'inProgress', 'items': [], 'error': None}}})",
    "        if 'SHOW_METHODS' in text:",
    "            out = json.dumps(methods)",
    "        else:",
    "            out = 'Applied fix.'",
    "        send({'method': 'item/completed', 'params': {'threadId': 'thr_review', 'turnId': turn_id, 'item': {'type': 'agentMessage', 'id': 'item_1', 'text': out}}})",
    "        send({'method': 'turn/completed', 'params': {'threadId': 'thr_review', 'turn': {'id': turn_id, 'status': 'completed', 'items': [], 'error': None}}})",
    "    else:",
    "        if req_id is not None:",
    "            send({'id': req_id, 'result': {}})"
  )
  writeLines(code, path)
  Sys.chmod(path, "755")
  path
}

test_that("chat_codex_review_loop() uses built-in review/start and fixes until clean", {
  codex_bin <- mock_codex_bin_review_loop()
  chat <- chat_codex(codex_bin = codex_bin, loadout = "standard", echo = "none")

  out <- chat_codex_review_loop(
    chat,
    max_iterations = 3L,
    is_clean = function(text) grepl("No issues found", text, fixed = TRUE)
  )

  expect_equal(out$status, "clean")
  expect_equal(out$iterations, 2L)
  expect_length(out$history, 2L)
  expect_match(out$history[[1]]$review, "Issue found", fixed = TRUE)
  expect_match(out$history[[1]]$fix, "Applied fix", fixed = TRUE)
  expect_true(isTRUE(out$history[[2]]$clean))

  methods <- jsonlite::parse_json(as.character(chat$chat("SHOW_METHODS")), simplifyVector = TRUE)
  expect_true(any(methods == "review/start"))
  expect_gte(sum(methods == "review/start"), 2)
})

test_that("chat_codex_review_loop() validates review target arguments", {
  codex_bin <- mock_codex_bin_review_loop()
  chat <- chat_codex(codex_bin = codex_bin, loadout = "standard", echo = "none")

  expect_error(
    chat_codex_review_loop(chat, target = "base_branch"),
    "base_branch"
  )
  expect_error(
    chat_codex_review_loop(chat, target = "commit"),
    "commit_sha"
  )
  expect_error(
    chat_codex_review_loop(chat, target = "custom"),
    "custom_instructions"
  )
})
