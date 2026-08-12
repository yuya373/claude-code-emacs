# CLAUDE.md

Use English for commit, code comment, pull request.

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs package that provides integration with Claude Code CLI. The package allows running Claude Code sessions within Emacs using vterm mode, with each project getting its own isolated session. Additionally, it includes an MCP (Model Context Protocol) server that enables Claude Code to interact directly with the Emacs environment.

## Development Commands

### Testing
```bash
# Run all tests (Emacs Lisp and TypeScript)
make test

# Run Emacs Lisp tests only
emacs -batch -l run-tests.el

# Run a specific Emacs test
emacs -batch -l run-tests.el -f ert-run-tests-batch-and-exit 'test-name-pattern'

# Run MCP server tests
npm test --prefix mcp-server

# Run specific MCP server test
npm test --prefix mcp-server -- --testNamePattern="test-pattern"
```

### Building
```bash
# Clean, compile, and test
make all

# Compile only
make compile

# Clean compiled files
make clean

# Install dependencies
make install-deps

# Build MCP server
make mcp-build

# Start MCP server in development mode (with hot-reload)
make mcp-dev
```

## Key Files and Entry Points

### Emacs Lisp
- `claude-code.el` - Main package entry point, loads all modules
- `claude-code-core.el` - Core utilities (chunking, error handling, session management)
- `claude-code-buffer.el` - Buffer naming and string processing
- `claude-code-commands.el` - Command execution (slash, custom, key sending)
- `claude-code-ui.el` - Transient menus and key bindings
- `claude-code-prompt.el` - Prompt file mode and operations
- `claude-code-mcp.el` - MCP WebSocket client integration
- `claude-code-mcp-connection.el` - WebSocket connection management
- `claude-code-mcp-protocol.el` - MCP protocol implementation
- `claude-code-mcp-tools.el` - MCP tool handlers
- `claude-code-mcp-events.el` - Event notification handlers
- `test-*.el` - Test files using ERT framework

### MCP Server (TypeScript)
- `mcp-server/src/index.ts` - Main server entry point
- `mcp-server/src/emacs-bridge.ts` - WebSocket server for Emacs communication
- `mcp-server/src/tools/*.ts` - Individual MCP tool implementations

## Architecture

### Session Management
Each project gets its own Claude Code buffer named `*claude:<project-root>*`. Key components:
- `claude-code-buffer-name()` - Generates unique buffer names per project
- `claude-code-ensure-buffer()` - Ensures buffer exists before operations
- `claude-code-with-vterm-buffer` - Helper macro for buffer context operations
- `claude-code-run()` - Starts or switches to Claude Code session
- `claude-code-close()` - Closes the window showing Claude Code buffer
- `claude-code-quit()` - Terminates session and kills buffer

### String Chunking System
Long strings are split into 50-character chunks to avoid terminal input limitations:
- `claude-code-chunk-string()` - Core chunking function
- Automatic delays between chunks for reliability
- Transparent to the user

### Custom Commands Architecture
Two types of custom commands are supported:

1. **Project Commands** (`.claude/commands/*.md`)
   - Sent as `/project:command-name`
   - Functions: `claude-code-execute-custom-command`
   - Interactive selection: `claude-code-get-custom-commands()`
   
2. **Global Commands** (`~/.claude/commands/*.md`)
   - Sent as `/user:command-name`
   - Functions: `claude-code-execute-global-command`
   - Interactive selection: `claude-code-get-global-commands()`

Both support `$ARGUMENTS` placeholders with interactive prompting via `claude-code-prompt-for-arguments()`.

### Macro Pattern for Slash Commands
Simple commands use this macro:
```elisp
(claude-code-define-slash-command "name" "/command")
```
This generates functions like `claude-code-init` that send `/init`.

### File Path Completion System
The `@` symbol triggers project file completion:
- `claude-code-at-sign-complete()` - Interactive completion using `projectile-project-files`
- Handles cases where `@` is already typed to avoid duplication

### Mode Architecture
- **`claude-code-vterm-mode`** - Minor mode for Claude Code vterm buffers
  - Parent: `vterm-mode`
  - Key bindings: Quick send commands, transient menu access
  - Auto-enabled when starting Claude Code session
- **`claude-code-prompt-mode`** - Major mode for `.claude-code.prompt.md` files
  - Parent: `markdown-mode`
  - Key bindings: Section/region sending, file completion
  - Auto-enabled for prompt files

### Transient Menu System
- Main menu: `claude-code-transient`
- Send menu: `claude-code-send-transient` - Send text/region to Claude Code
- Insert menu: `claude-code-insert-transient` - Insert file paths to prompt buffer
- Slash commands menu: `claude-code-slash-commands-transient` - All Claude Code slash commands
- Git & GitHub menu: `claude-code-git-menu-transient` - Git-related commands
- Prompt menu: `claude-code-prompt-transient`
- Organized into logical groups with mnemonic key bindings

## Testing Strategy

Tests use mock implementations to avoid vterm dependencies:
- Mock vterm functions with `cl-letf`
- Test data flows rather than terminal interactions
- Integration tests verify complete workflows
- Each module has its own test file (`test-claude-code-*.el`)
- Run all tests: `make test` or `emacs -batch -l run-tests.el`
- Run specific test: `emacs -batch -l run-tests.el -f ert-run-tests-batch-and-exit 'pattern'`

## CI/CD
- GitHub Actions runs tests on push/PR
- Tests against Emacs 28.1, 29.1, and snapshot
- Special handling for vterm module compilation in CI

## Important Implementation Details

### Error Handling
- Always use `claude-code-ensure-buffer` before operations
- Check file existence before reading command files
- Validate arguments are non-empty when required

### Keybinding Conventions
- `C-c C-*` in prompt buffers for mode-specific commands
- Single letters in transient menus for quick access
- `y` as alias for `1` (yes responses)
- `@` triggers file path completion in prompt buffers

### Dependencies
- **Required**: projectile, vterm, transient, markdown-mode, websocket
- **Optional**: lsp-mode (for diagnostics and language ID configuration)

### Coding Standards
- Use lexical binding in all files
- Prefix all functions with `claude-code-`
- Use defcustom for user-configurable variables
- Document all public functions
- Add unit tests for new functionality

### Common Lisp Pitfalls
- **Avoid quoted lists for mutable data**: Never use `'((key . value))` for data that will be modified with `setcdr`, `setcar`, etc. Quoted lists are constants and shared across all uses.
  ```elisp
  ;; BAD - all calls share the same list object
  (defun make-info () '((websocket . nil)))
  
  ;; GOOD - creates a new list each time
  (defun make-info () (list (cons 'websocket nil)))
  ```
- **Use `list` and `cons` for creating fresh data structures**: This ensures each call creates independent objects that can be safely modified.

When modifying this package:
1. Add tests for new functionality
2. Use the established macro patterns for new commands
3. Maintain project isolation in buffer naming
4. Follow the chunking pattern for long strings
5. Update relevant documentation (README, CLAUDE.md)


## MCP Server

### Architecture
The MCP server provides a bridge between Claude Code and Emacs:
- WebSocket server on dynamic port for Emacs connection
- stdio interface for Claude Code MCP protocol
- Implements tools: getOpenBuffers, getCurrentSelection, getDiagnostics, getDefinition, findReferences, describeSymbol, diff tools (openDiffFile, openRevisionDiff, openCurrentChanges, openDiffContent), sendNotification
- Implements resources: buffer content, project info
- Per-server-instance WebSocket connections: each MCP server process generates a unique instance ID, so multiple Claude Code sessions (agents) in the same project hold independent connections
- Real-time event notifications from Emacs to Claude Code (broadcast to every instance of the project)

### How MCP Connection Works
1. Claude Code automatically starts the MCP server when you begin a session (no need to type `/mcp`)
2. MCP server generates a unique instance ID and creates a WebSocket server on a dynamic port
3. MCP server calls `claude-code-mcp-register-port` (project root, port, instance ID) via emacsclient
4. Emacs stores the connection keyed by instance ID and establishes the WebSocket connection
5. Connection is now ready for bidirectional communication
6. On shutdown the server calls `claude-code-mcp-unregister-port` with its instance ID, so only its own connection is removed — other agents in the same project stay connected
7. If the connection drops without a server shutdown (e.g. Emacs restarts — background sessions and agents hosted by the claude daemon outlive Emacs), the server re-registers via emacsclient with exponential backoff (5s up to 60s, giving up after 120 attempts ≈ 2 hours) until Emacs connects back (see `mcp-server/src/reconnect.ts`). A normal WebSocket closure (code 1000, i.e. Emacs deliberately disconnected) is not retried. A server-side heartbeat (30s ping/pong) detects half-open sockets left by an Emacs that died without closing the connection

**Important**: Claude Code must be configured with the MCP server (see Setup section) for the features to work.

### Setup
See [README.md Installation Details](README.md#installation-details) for MCP server setup options (global install, npx, or build from source).

### Logging
The MCP server logs to a file for debugging purposes:
- Log file location: `.claude-code-mcp.log` in project root
- Logs include timestamps, connection status, and request/response details
- Event notifications are logged with full parameters
- Useful for troubleshooting MCP integration issues

### Development
When working on the MCP server:
1. Build with `make mcp-build`
2. Check logs for debugging: `tail -f .claude-code-mcp.log`
3. Test TypeScript code: `npm test --prefix mcp-server`
4. The server auto-starts when Claude Code requests MCP tools
5. For development with hot-reload: `make mcp-dev`

### Event Notification System
The MCP server supports real-time event notifications from Emacs to Claude Code.
Events include buffer list updates, content changes, and LSP diagnostics updates.
See `claude-code-mcp-events.el` for implementation details.
