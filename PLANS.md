# Implement `chat_codex()` Using Codex App Server With Ellmer Tool Bridging

This ExecPlan is a living document. The sections `Progress`, `Surprises & Discoveries`, `Decision Log`, and `Outcomes & Retrospective` must be kept up to date as work proceeds.

This repository currently does not include a separate planning standard file. This file is the authoritative implementation handoff for this feature.

Implementation policy for this plan: use strict TDD. For every behavior change, write or update a failing test first (red), implement the minimal code to pass (green), and only then commit. Every feature commit must include the tests that exercise that feature and must correspond to a passing test run.

## Purpose / Big Picture

After this change, an ellmer user can create a chat object backed by the local Codex CLI login (including ChatGPT subscription authentication) and use it through normal ellmer chat methods. The user can call `chat_codex()` and use `chat$chat()`, `chat$stream()`, and `chat$chat_structured()` without managing API keys.

The most important user-visible behavior is tool interoperability: tools registered with `chat$register_tool()` must be visible to the Codex agent during the turn, and tool execution must happen in R through ellmer's tool definitions. This enables Codex thread persistence and Codex agent behavior while still using ellmer tools.

Success is demonstrated by a full round trip where:

1. `chat_codex()` starts a Codex app-server session.
2. A user prompt triggers a Codex dynamic tool call.
3. The tool executes in R.
4. Codex continues the same turn and returns a final assistant answer that incorporates tool output.

## Progress

- [x] (2026-02-16 00:00Z) Completed architecture investigation of ellmer provider internals and Codex app-server protocol internals.
- [ ] Implement transport abstraction so providers are not hard-wired to `httr2` request/response only.
- [ ] Implement `chat_codex()` and `ProviderCodex` runtime state management.
- [ ] Implement Codex thread/turn execution path with text streaming bridge.
- [ ] Implement dynamic tool bridge (`dynamicTools` + `item/tool/call` handling).
- [ ] Implement structured output bridge (`turn/start.outputSchema`).
- [ ] Add tests with a deterministic mock app-server process.
- [ ] Add docs and explicit unsupported-feature behavior.

## Surprises & Discoveries

- Observation: Ellmer provider execution currently assumes HTTP transport end-to-end.
  Evidence: `R/httr2.R` `chat_perform()` always builds and performs an `httr2` request, and `R/chat.R` consumes results as HTTP responses.

- Observation: Ellmer tool execution is currently post-response, driven by provider-emitted `ContentToolRequest` items.
  Evidence: `R/chat.R` invokes `invoke_tools()` only after `submit_turns()` finishes and produces an assistant turn.

- Observation: Codex app-server can request client actions in the middle of a turn and requires bidirectional JSON-RPC handling.
  Evidence: server-initiated requests include `item/tool/call`, `item/commandExecution/requestApproval`, and `item/fileChange/requestApproval`.

- Observation: Codex dynamic tools are currently behind `experimentalApi`, but are persisted and restored in Codex thread state.
  Evidence: protocol marks `thread/start.dynamicTools` as experimental, and Codex core reads persisted dynamic tools on resume/fork.

- Observation: The local `codex` binary in this environment supports `app-server`.
  Evidence: `codex --version` reports `codex-cli 0.101.0`; `codex app-server --help` shows the app-server command surface.

- Observation: Ellmer currently has no process finalizers or unload cleanup for long-lived child processes.
  Evidence: no `finalize`, `reg.finalizer`, or `.onUnload` process cleanup in `R/chat.R` or `R/zzz.R`.

## Decision Log

- Decision: Integrate via Codex app-server, not `codex exec`, and not the TypeScript SDK wrapper.
  Rationale: App-server is the official stable harness protocol for rich clients and is the only path that supports in-turn dynamic tool callbacks and thread lifecycle control from R.
  Date/Author: 2026-02-16 / Codex

- Decision: Keep one long-lived app-server child process per `Chat` instance (provider runtime), not one process per turn.
  Rationale: Preserves turn/thread continuity, avoids repeated startup/auth overhead, and matches app-server lifecycle expectations.
  Date/Author: 2026-02-16 / Codex

- Decision: Codex owns the tool loop for this provider; ellmer hosts tool execution.
  Rationale: Codex tool calls occur during an active turn via server requests; ellmer's existing post-response loop cannot represent this timing without major semantic mismatch.
  Date/Author: 2026-02-16 / Codex

- Decision: First release supports synchronous `chat`, `stream`, and `chat_structured`; async and batch/parallel APIs fail fast with explicit errors.
  Rationale: Keeps scope feasible while delivering core behavior and prevents silent misbehavior in unsupported paths.
  Date/Author: 2026-02-16 / Codex

- Decision: Stream bridge exposes assistant text and optional thinking deltas through existing ellmer stream primitives; richer app-server events remain provider-internal or console progress in v1.
  Rationale: Natural mapping exists for text/thinking; command/file/mcp item lifecycles do not map cleanly to existing ellmer stream output contracts.
  Date/Author: 2026-02-16 / Codex

## Outcomes & Retrospective

This plan captures investigation findings and a concrete implementation sequence, but code changes are not started yet. The design intentionally prioritizes correctness of protocol handling and tool integration over broad API parity. The largest technical risk is the current `httr2`-centric transport assumption in ellmer core, so milestone 1 explicitly introduces a provider transport extension seam before Codex provider code is added.

## Context and Orientation

Ellmer today organizes providers as S7 subclasses of `Provider` in `R/provider-*.R`. The `Chat` R6 class in `R/chat.R` builds turns, calls `chat_perform()`, streams provider chunks, constructs an assistant turn, then optionally runs local tools.

Key files and what they currently do:

- `R/provider.R`: provider base class and provider generics like `chat_request`, `stream_content`, `value_turn`, and `as_json`.
- `R/httr2.R`: `chat_perform()` runtime execution path, currently HTTP-only.
- `R/chat.R`: orchestrates turn submission, streaming behavior, and post-response tool loop.
- `R/chat-tools.R`: tool invocation (`invoke_tools`, `invoke_tool`, callbacks, conversion).
- `R/content.R` and `R/turns.R`: content and turn object model.
- `R/provider-openai.R` and `R/provider-openai-compatible.R`: best concrete examples of a fully integrated provider.

Terms used in this plan:

- App-server: Codex CLI subcommand (`codex app-server`) that speaks bidirectional JSON-RPC over JSON lines.
- Thread: a durable Codex conversation id.
- Turn: one user request and agent work sequence.
- Dynamic tools: client-provided tool schemas on `thread/start`; Codex asks the client to execute them via `item/tool/call`.
- Message pump: a loop reading app-server stdout lines, parsing JSON-RPC messages, routing responses, notifications, and server requests.

## Plan of Work

### Milestone 1: Add a provider transport seam so non-HTTP runtimes are possible

At the end of this milestone, ellmer can support a provider that does not use `httr2` request objects for execution. Existing providers remain unchanged.

Edit `R/provider.R` to add two new provider-level execution generics and defaults:

- a generic for provider execution runtime (synchronous and streaming modes),
- a generic for extracting value-mode response payload and duration.

The default methods delegate to current `httr2` behavior so existing providers are preserved.

Edit `R/httr2.R` to route `chat_perform()` through the new provider execution generic first, then keep current HTTP behavior as the default path.

Edit `R/chat.R` only where value-mode results are decoded (`resp_body_json` and timing extraction), replacing hard-coded HTTP extraction with provider generic calls so non-HTTP providers can return provider-native result envelopes.

Acceptance for milestone 1:

- Existing provider tests continue to pass.
- No user-visible API changes yet.

### Milestone 2: Add `chat_codex()` provider and process-backed runtime

At the end of this milestone, users can create `chat_codex()` and send basic prompts.

Add new file `R/provider-codex.R` containing:

- `chat_codex(...)` exported constructor.
- `ProviderCodex` S7 class.
- runtime state helpers (process handle, request id counter, thread id, initialization flag, pending map, buffer queue).
- app-server spawn/init helpers.
- JSON-RPC send/read helpers.

Provider constructor requirements:

- Use local `codex` binary path (default `"codex"`), configurable via argument.
- Use official app-server handshake (`initialize` then `initialized`).
- Use app-server thread lifecycle (`thread/start`, `thread/resume` when needed).
- Do not scrape internal rollout files; only use protocol methods.

State lifecycle:

- One process per chat/provider runtime.
- Process persists across turns.
- If process dies, respawn and resume by saved thread id when available.
- Add cleanup:
  - `finalize` in `Chat` class to stop provider runtime when provider is `ProviderCodex`.
  - `.onUnload` cleanup in `R/zzz.R` for any leaked Codex processes.

Acceptance for milestone 2:

- `chat_codex()` object prints like other providers.
- First `chat$chat("...")` returns assistant text through Codex app-server.
- Second `chat$chat("...")` continues same Codex thread.

### Milestone 3: Implement streaming bridge and turn assembly

At the end of this milestone, `chat$stream()` works for Codex text output.

Implement `ProviderCodex` methods for:

- runtime execution in stream mode: pump notifications in real time and yield provider chunks.
- `stream_content`: map `item/agentMessage/delta` to `ContentText`.
- optional thinking bridge: map `item/reasoning/summaryTextDelta` to `ContentThinking` when `stream = "content"`.
- `stream_merge_chunks`: aggregate item lifecycle and final turn status until `turn/completed`.
- `value_turn`: create `AssistantTurn` from aggregated completed items.

Behavior details:

- Treat `item/completed` as authoritative item state.
- Treat `turn/completed` as terminal status signal.
- Surface turn failures with clear `cli_abort()` messages containing server-provided error message and code info when available.

Acceptance for milestone 3:

- `chat$chat()` works (non-stream).
- `chat$stream()` yields text chunks in real time for assistant message deltas.
- `chat$stream(stream = "content")` can include `ContentThinking` chunks when available.

### Milestone 4: Implement dynamic tool bridge for registered ellmer tools

At the end of this milestone, ellmer tools registered on the chat are available to Codex and execute in R during the active turn.

Protocol behavior:

- On first thread start, include `dynamicTools` derived from currently registered ellmer `ToolDef`s.
- Opt in to `experimentalApi` during `initialize` because dynamic tools are gated.
- Handle server request `item/tool/call`:
  - resolve tool by name from registered tools.
  - invoke using existing ellmer logic (`invoke_tool` path).
  - map result to app-server `DynamicToolCallResponse`.

Tool result mapping:

- Successful scalar/text/json results -> `inputText`.
- `ContentImageRemote` -> `inputImage` with URL.
- `ContentImageInline` -> `inputImage` with `data:` URL.
- Unsupported content types -> fail fast with explicit tool error text.

Callback behavior:

- Reuse `on_tool_request` and `on_tool_result` callback surfaces by creating compatible request/result objects around dynamic calls.

Tool mutability rule for v1:

- Tool set is frozen after first thread start.
- Calling `register_tool`/`set_tools` after thread start raises a clear error for `ProviderCodex`.
- Rationale: app-server dynamic tools are thread-start scoped in the stable flow.

Acceptance for milestone 4:

- A prompt that triggers a registered tool completes in one Codex turn and includes tool output in the final answer.
- Tool callbacks fire.
- Unknown tool names return tool error response and do not crash the runtime.

### Milestone 5: Structured output bridge, unsupported-feature guards, tests, and docs

At the end of this milestone, the first complete and documented release behavior exists.

Structured output:

- Implement `chat_structured()` support by sending `turn/start.outputSchema` from `as_json(provider, type)` schema output.
- Parse final assistant item text into `ContentJson` for existing `extract_data()` flow.

Unsupported features in v1 must fail fast:

- `chat_async()`
- `stream_async()`
- `chat_structured_async()`
- batch chat and parallel chat paths for `ProviderCodex`

Add explicit errors that state feature is not yet supported for `chat_codex`.

Tests:

- Add `tests/testthat/test-provider-codex.R`.
- Add a deterministic mock app-server helper process in `tests/testthat/helper-codex-mock.R` (or equivalent script) that emulates:
  - initialize handshake,
  - thread start/turn start,
  - streaming text deltas,
  - dynamic tool request and response flow,
  - turn completed and error paths.
- Add snapshot for constructor defaults and failure messages.

Docs:

- Add roxygen docs for `chat_codex()`.
- Update `DESCRIPTION` and collate order.
- Mention known limitations similar to other provider docs.

Acceptance for milestone 5:

- Tests verify text chat, streaming, structured output, and dynamic tools.
- Unsupported APIs error clearly.
- Provider is discoverable via `chat("codex/<model>")` when function signature includes required fields.

## Concrete Steps

All commands below run from repository root: `/Users/tomasz/github/tidyverse/ellmer`.

1. Implement milestone 1 transport seam.

    R -q -e "devtools::test('test-provider-openai.R')"
    R -q -e "devtools::test('test-chat.R')"

Expected:

    Existing tests in touched chat/provider internals pass without behavior changes.

2. Implement `ProviderCodex` runtime and constructor (milestones 2-3 partial).

    R -q -e "devtools::test('test-provider-codex.R')"

Expected:

    Constructor snapshot passes.
    Basic chat and stream tests against mock app-server pass.

3. Implement dynamic tool bridge (milestone 4).

    R -q -e "devtools::test('test-provider-codex.R')"
    R -q -e "devtools::test('test-chat-tools.R')"

Expected:

    New codex-specific tool tests pass.
    Existing tool tests remain green.

4. Add structured output and unsupported-feature guards (milestone 5).

    R -q -e "devtools::test('test-provider-codex.R')"
    R -q -e "devtools::test()"

Expected:

    Codex provider tests pass.
    Full suite passes or failures are unrelated and documented in this file.

5. Regenerate docs when exports are added.

    R -q -e "devtools::document()"

Expected:

    `NAMESPACE` and man files include `chat_codex`.

## Validation and Acceptance

The final implementation is accepted when all behaviors below are observable:

1. Basic chat behavior:

    chat <- chat_codex(echo = "none")
    out1 <- chat$chat("Reply with exactly: OK")
    out2 <- chat$chat("Now reply with exactly: STILL_OK")

Observe:

- `out1` includes `OK`.
- `out2` includes `STILL_OK`.
- second response reflects thread continuity, not a fresh session.

2. Streaming behavior:

    chunks <- coro::collect(chat$stream("Count to three with commas."))
    paste0(unlist(chunks), collapse = "")

Observe:

- Stream yields multiple chunks (not only one final message).
- Final concatenated text matches expected content.

3. Tool bridge behavior:

    chat$register_tool(tool(function() "2024-01-01", name = "current_date", description = "Return current date"))
    out <- chat$chat("Use current_date and report it.")

Observe:

- Tool request/response path executes in R.
- Final answer includes `2024-01-01`.

4. Structured output:

    out <- chat$chat_structured("Extract number: eleven", type = type_number())

Observe:

- Returns numeric `11`.

5. Unsupported feature clarity:

    chat$chat_async("hello")

Observe:

- Immediate, explicit error saying async is not yet supported for `chat_codex`.

## Idempotence and Recovery

Implementation steps are additive and should be repeatable.

Safe retry guidance:

- If mock app-server tests fail due to stale process state, terminate spawned mock processes and re-run tests.
- If codex runtime process dies mid-turn, provider runtime must respawn app-server and either:
  - resume stored thread id, or
  - fail fast with a clear error if resume fails.

Runtime safety rules:

- Never leave orphaned app-server processes on normal object cleanup.
- On package unload, terminate remaining codex child processes.
- Do not delete or mutate Codex rollout/session files directly; use protocol methods only.

## Artifacts and Notes

Expected handshake message shape:

    {"method":"initialize","id":1,"params":{"clientInfo":{"name":"ellmer","title":"ellmer","version":"<pkg-version>"},"capabilities":{"experimentalApi":true}}}
    {"method":"initialized","params":{}}

Expected turn flow skeleton:

    {"method":"thread/start","id":2,"params":{...}}
    {"id":2,"result":{"thread":{"id":"thr_123"}}}
    {"method":"turn/start","id":3,"params":{"threadId":"thr_123","input":[{"type":"text","text":"..."}]}}
    {"method":"item/agentMessage/delta","params":{"itemId":"item_1","delta":"Hello"}}
    {"method":"turn/completed","params":{"turn":{"id":"turn_1","status":"completed"}}}

Expected dynamic tool call skeleton:

    {"method":"item/tool/call","id":40,"params":{"threadId":"thr_123","turnId":"turn_1","callId":"call_1","tool":"current_date","arguments":{}}}
    {"id":40,"result":{"contentItems":[{"type":"inputText","text":"2024-01-01"}],"success":true}}

## Interfaces and Dependencies

Dependencies:

- Add `processx` to `DESCRIPTION` Imports for child process management and stdio IO.
- Keep `jsonlite`, `coro`, and existing ellmer dependencies for parsing and generators.

New public API:

- `chat_codex(...)` in `R/provider-codex.R`.

Proposed constructor signature:

    chat_codex(
      system_prompt = NULL,
      model = NULL,
      params = NULL,
      echo = c("none", "output", "all"),
      codex_bin = "codex",
      cwd = NULL,
      approval_policy = c("on-request", "unless-trusted", "never"),
      sandbox = c("read-only", "workspace-write", "danger-full-access"),
      experimental_api = TRUE
    )

New provider class:

- `ProviderCodex` extends `Provider`.
- Additional state fields include runtime process/session state.

New/updated internal interfaces:

- Provider runtime generic in `R/provider.R` for custom execution transport.
- Provider response extraction generics in `R/provider.R` for value body and duration.
- `ProviderCodex` implementations for:
  - runtime execution,
  - stream mapping,
  - turn assembly,
  - structured output schema pass-through.

Files expected to change:

- `DESCRIPTION`
- `R/provider.R`
- `R/httr2.R`
- `R/chat.R`
- `R/provider-codex.R` (new)
- `R/zzz.R`
- `tests/testthat/helper-provider.R` (if shared helper needs codex hooks)
- `tests/testthat/test-provider-codex.R` (new)
- Snapshot files under `tests/testthat/_snaps/` as needed
- generated docs files (`NAMESPACE`, `man/chat_codex.Rd`) after `devtools::document()`

## Revision Note

Initial version created on 2026-02-16 to hand off a full implementation plan for adding a Codex app-server-backed ellmer provider with dynamic tool interoperability. This revision records investigation findings and resolves initial architecture choices before coding starts.
