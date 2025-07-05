# Claude Code Native UI Prototype

## Overview

This is a prototype implementation of a native Emacs interface for Claude Code, replacing the current vterm-based approach. The idea was proposed by @dwcoates in [Issue #4](https://github.com/yuya373/claude-code-emacs/issues/4).

## Motivation

The current vterm-based implementation has several limitations:
- Terminal interface restrictions (chunking required for long strings)
- Limited editing capabilities within vterm
- Visual artifacts and flickering
- Cannot leverage full Emacs capabilities

A native UI would provide:
- Fully editable buffers
- Clean visual presentation without terminal artifacts
- Deep integration possibilities with Emacs tools (Magit, etc.)
- Better performance and responsiveness

## Architecture

### Key Components

1. **Process Management**
   - Uses `make-process` to run Claude CLI directly
   - Command: `claude -p "prompt" --output-format stream-json --verbose`
   - Handles stdin/stdout communication without terminal emulation

2. **Session Management**
   - Session IDs are extracted from the initial `system` message
   - Stored in both buffer-local variables and global hash table
   - Supports `--resume` for continuing conversations

3. **JSON Stream Processing**
   - Claude SDK outputs newline-delimited JSON with `--output-format stream-json`
   - Messages types: `system`, `assistant`, `user`, `result`
   - Handles vectors/arrays properly (Emacs 27.1+ returns vectors from `json-parse-string`)

4. **UI Components**
   - Based on `markdown-mode` for syntax highlighting
   - Custom faces for different message types
   - Shows session info, costs, execution times
   - Displays MCP server connections

## Implementation Details

### Message Types (from SDKMessage.ts)

```typescript
type SDKMessage =
  | { type: "system"; subtype: "init"; session_id: string; ... }
  | { type: "assistant"; message: Message; session_id: string; }
  | { type: "user"; message: MessageParam; session_id: string; }
  | { type: "result"; subtype: "success" | "error_*"; ... }
```

### Key Functions

- `claude-code-native-send-prompt`: Main entry point for sending prompts
- `claude-code-native-process-filter`: Handles streaming JSON output
- `claude-code-native-handle-json-line`: Parses individual JSON messages
- `claude-code-native-get-session-id`: Manages session persistence

### Current Limitations

1. **No slash commands** - SDK mode doesn't support `/init`, `/help`, etc.
2. **No streaming display** - Messages appear all at once (could be improved)
3. **Basic UI** - Minimal formatting, could be enhanced
4. **Tool execution** - Shows tool use but not detailed results

## Usage

```elisp
;; Load and start
(load-file "claude-code-emacs-native.el")
(claude-code-native)

;; Key bindings in claude-code-native-mode:
;; C-c C-c - Send region or prompt
;; C-c C-p - Send new prompt
;; C-c C-k - Kill current process
;; C-c C-n - Start new session

;; Debug mode
(setq claude-code-native-debug t)
```

## Future Enhancements

1. **Streaming Support**
   - Display text as it arrives instead of buffering
   - Show typing indicators

2. **Slash Command Emulation**
   - Implement common commands like `/init`, `/clear`
   - Read `.claude/instructions.md` for initialization

3. **Enhanced UI**
   - Sidebar for session management
   - Better tool execution display
   - Inline images and diagrams
   - Collapsible sections

4. **Integration Features**
   - Direct Magit integration
   - LSP integration for code context
   - Project-wide search and replace

5. **Performance**
   - Async processing for better responsiveness
   - Efficient handling of large outputs

## Testing

```elisp
;; Check Claude CLI installation
(load-file "test-native-demo.el")
(claude-code-native-check-setup)

;; Test streaming parsing
(claude-code-native-test-stream-parsing)

;; Toggle debug mode
(claude-code-native-toggle-debug)
```

## Experimental Versions

Several experimental versions were created to explore streaming JSON input capabilities:

### Version 2-5: Streaming JSON Input Attempts

Multiple approaches were tested to keep the Claude process alive between messages:

1. **v2** - Attempted to use `--input-format stream-json` with persistent process
2. **v3** - Tried interactive mode without `-p` flag  
3. **v4** - Used `start-process-shell-command` with echo pipe
4. **v5** - Attempted shell process with persistent stdin

### Key Findings

Unfortunately, the `-p` (print) flag has a critical limitation:
- `-p` means "Print response and exit"
- The process always terminates after producing output
- Streaming JSON input is designed for batch processing, not persistent connections

### Technical Details

```elisp
;; This always exits after response:
claude -p --output-format=stream-json --input-format=stream-json

;; Streaming JSON input allows multiple messages in one invocation:
echo '{"type":"user","message":{...}}
{"type":"user","message":{...}}' | claude -p --input-format=stream-json
```

### Conclusion on Streaming

While streaming JSON input is useful for sending multiple messages in a single invocation, it does not support maintaining a persistent process. The v1 approach (new process per message) remains the most reliable solution.

## Recommendation

Based on extensive experimentation:

1. **Use v1 approach** - Create a new process for each message
   - Simple and reliable
   - Session persistence via `--resume` flag
   - No complex state management needed

2. **Streaming JSON is not suitable for persistent processes**
   - The `-p` flag always causes process termination
   - Designed for batch processing, not interactive use

3. **Future possibilities**
   - Interactive mode without `-p` (complex but possible)
   - WebSocket or HTTP API if available
   - Custom protocol implementation

## Conclusion

This prototype demonstrates that a native Emacs UI for Claude Code is not only feasible but offers significant advantages over the terminal-based approach. While we cannot maintain a persistent process with the current Claude CLI, the v1 implementation provides a clean, functional interface that:

- Eliminates vterm limitations
- Provides full Emacs editing capabilities
- Maintains session context across messages
- Offers better visual presentation
- Enables future enhancements

The implementation is straightforward, leveraging standard Emacs features like `make-process`, JSON parsing, and buffer management. With further development, this could become a powerful alternative interface for Claude Code users who prefer native Emacs integration.
