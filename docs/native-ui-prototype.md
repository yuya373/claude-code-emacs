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

## Version 2: Streaming JSON Input

A second version (`claude-code-emacs-native-v2.el`) implements streaming JSON input, which offers several advantages:

### Key Improvements

1. **Persistent Process** - Process stays alive between prompts
2. **Continuous Conversation** - No need to restart for each message
3. **Better Performance** - Reduced overhead from process creation
4. **True Streaming** - Can send messages while Claude is still processing

### Implementation

```elisp
;; Process starts once and stays alive
(make-process
  :command (list "claude" "-p" 
                 "--output-format" "stream-json"
                 "--input-format" "stream-json"  ; Key addition
                 "--verbose")
  ...)

;; Send messages via stdin
(process-send-string process 
  "{\"type\":\"user\",\"message\":{...}}\n")
```

### Usage

```elisp
(load-file "claude-code-emacs-native-v2.el")
(claude-code-native-v2)
;; Process starts on first message and stays alive
```

## Conclusion

This prototype demonstrates that a native Emacs UI for Claude Code is not only feasible but offers significant advantages over the terminal-based approach. The SDK provides all necessary functionality, and Emacs's capabilities can be fully leveraged for a superior user experience.

The implementation is straightforward, leveraging standard Emacs features like `make-process`, JSON parsing, and buffer management. With further development, this could become a powerful alternative interface for Claude Code users who prefer native Emacs integration.

The streaming JSON input approach (v2) shows even more promise for building a responsive, efficient interface that maintains context across multiple interactions without the overhead of process management.
