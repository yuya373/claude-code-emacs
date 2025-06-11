# CLAUDE.md

Use English for commit, code comment, pull request.

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs package that provides integration with Claude Code CLI. The package allows running Claude Code sessions within Emacs using vterm mode, with each project getting its own isolated session. Additionally, it includes an MCP (Model Context Protocol) server that enables Claude Code to interact directly with the Emacs environment.

## Package Structure

The package is modularized into several components:
- **claude-code-emacs.el** - Main entry point
- **claude-code-emacs-core.el** - Core functionality, utilities, and session management
- **claude-code-emacs-buffer.el** - Buffer naming and string processing
- **claude-code-emacs-commands.el** - Command execution and slash commands
- **claude-code-emacs-ui.el** - Transient menu interfaces
- **claude-code-emacs-prompt.el** - Prompt file management and mode
- **claude-code-emacs-mcp.el** - MCP WebSocket client integration
- **claude-code-emacs-mcp-*.el** - MCP protocol implementation

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

### Linting and Type Checking
When completing tasks, run these commands to ensure code quality:
```bash
# For Emacs Lisp files, byte-compilation serves as linting
make compile

# For TypeScript files in MCP server
cd mcp-server && npm run build

# Run all tests to ensure functionality
make test
```

## Key Files and Entry Points

### Emacs Lisp
- `claude-code-emacs.el` - Main package entry point, loads all modules
- `claude-code-emacs-core.el` - Core utilities (chunking, error handling, session management)
- `claude-code-emacs-buffer.el` - Buffer naming and string processing
- `claude-code-emacs-commands.el` - Command execution (slash, custom, key sending)
- `claude-code-emacs-ui.el` - Transient menus and key bindings
- `claude-code-emacs-prompt.el` - Prompt file mode and operations
- `claude-code-emacs-mcp.el` - MCP WebSocket client integration
- `claude-code-emacs-mcp-connection.el` - WebSocket connection management
- `claude-code-emacs-mcp-protocol.el` - MCP protocol implementation
- `claude-code-emacs-mcp-tools.el` - MCP tool handlers
- `test-*.el` - Test files using ERT framework

### MCP Server (TypeScript)
- `mcp-server/src/index.ts` - Main server entry point
- `mcp-server/src/emacs-bridge.ts` - WebSocket server for Emacs communication
- `mcp-server/src/tools/*.ts` - Individual MCP tool implementations
  - `command-tools.ts` - runCommand tool using emacsclient subprocess

## Architecture

### Session Management
Each project gets its own Claude Code buffer named `*claude:<project-root>*`. Key components:
- `claude-code-emacs-buffer-name()` - Generates unique buffer names per project
- `claude-code-emacs-ensure-buffer()` - Ensures buffer exists before operations
- `claude-code-emacs-with-vterm-buffer` - Helper macro for buffer context operations
- `claude-code-emacs-run()` - Starts or switches to Claude Code session
- `claude-code-emacs-close()` - Closes the window showing Claude Code buffer
- `claude-code-emacs-quit()` - Terminates session and kills buffer

### String Chunking System
Long strings are split into 50-character chunks to avoid terminal input limitations:
- `claude-code-emacs-chunk-string()` - Core chunking function
- Automatic delays between chunks for reliability
- Transparent to the user

### Custom Commands Architecture
Two types of custom commands are supported:

1. **Project Commands** (`.claude/commands/*.md`)
   - Sent as `/project:command-name`
   - Functions: `claude-code-emacs-execute-custom-command`
   - Interactive selection: `claude-code-emacs-get-custom-commands()`
   
2. **Global Commands** (`~/.claude/commands/*.md`)
   - Sent as `/user:command-name`
   - Functions: `claude-code-emacs-execute-global-command`
   - Interactive selection: `claude-code-emacs-get-global-commands()`

Both support `$ARGUMENTS` placeholders with interactive prompting via `claude-code-emacs-prompt-for-arguments()`.

### Macro Pattern for Slash Commands
Simple commands use this macro:
```elisp
(claude-code-emacs-define-slash-command "name" "/command")
```
This generates functions like `claude-code-emacs-init` that send `/init`.

### File Path Completion System
The `@` symbol triggers project file completion:
- `claude-code-emacs-get-buffer-paths()` - Lists all open project files
- `claude-code-emacs-at-sign-complete()` - Interactive completion
- Project root is replaced with `@` for brevity

### Mode Architecture
- **`claude-code-emacs-vterm-mode`** - Minor mode for Claude Code vterm buffers
  - Parent: `vterm-mode`
  - Key bindings: Quick send commands, transient menu access
  - Auto-enabled when starting Claude Code session
- **`claude-code-emacs-prompt-mode`** - Major mode for `.claude-code-emacs.prompt.md` files
  - Parent: `markdown-mode`
  - Key bindings: Section/region sending, file completion
  - Auto-enabled for prompt files

### Transient Menu System
- Main menu: `claude-code-emacs-transient`
- Prompt menu: `claude-code-emacs-prompt-transient`
- Organized into logical groups with mnemonic key bindings

## Testing Strategy

Tests use mock implementations to avoid vterm dependencies:
- Mock vterm functions with `cl-letf`
- Test data flows rather than terminal interactions
- Integration tests verify complete workflows
- Each module has its own test file (`test-claude-code-emacs-*.el`)
- Run all tests: `make test` or `emacs -batch -l run-tests.el`
- Run specific test: `emacs -batch -l run-tests.el -f ert-run-tests-batch-and-exit 'pattern'`

## CI/CD
- GitHub Actions runs tests on push/PR
- Tests against Emacs 28.1, 29.1, and snapshot
- Special handling for vterm module compilation in CI

## Important Implementation Details

### Error Handling
- Always use `claude-code-emacs-ensure-buffer` before operations
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
- Prefix all functions with `claude-code-emacs-`
- Use defcustom for user-configurable variables
- Document all public functions
- Add unit tests for new functionality

When modifying this package:
1. Add tests for new functionality
2. Use the established macro patterns for new commands
3. Maintain project isolation in buffer naming
4. Follow the chunking pattern for long strings
5. Update relevant documentation (README, CLAUDE.md)

### Recent Changes
- **getDefinition tool**: Added MCP tool to find symbol definitions using LSP with preview (shows 3 lines before/after)
- **Diff tools suite**: Added comprehensive ediff integration tools:
  - `openDiff`: Compare two files or buffers
  - `openDiff3`: Three-way file comparison for merge conflicts
  - `openRevisionDiff`: Compare file with any git revision
  - `openCurrentChanges`: Show uncommitted changes in ediff
  - `applyPatch`: Apply patch files using ediff
- **Interactive claude-code-emacs-run**: Added prefix argument support (`C-u`) for interactive option selection (model, verbose, resume, etc.)
- **CI/CD**: Added GitHub Actions workflow for automated testing
- **getDiagnostics simplified**: Removed bufferPath parameter, now always returns project-wide diagnostics
- **runCommand tool**: Executes Emacs commands via emacsclient subprocess (no longer uses WebSocket)
- **MCP Resources**: Added support for MCP resources (buffer content, project info, diagnostics)
- **Enhanced logging**: MCP server now logs to project root (`.claude-code-emacs-mcp.log`)
- **Shift+Tab support**: Added `claude-code-emacs-send-shift-tab` to toggle auto accept
- **Function rename**: `claude-code-emacs-send-buffer-or-region` → `claude-code-emacs-send-region`
- **Module consolidation**: Session management moved from separate module into core.el

## MCP Server

### Architecture
The MCP server provides a bridge between Claude Code and Emacs:
- WebSocket server on dynamic port for Emacs connection
- stdio interface for Claude Code MCP protocol
- Implements tools: openFile, getOpenBuffers, getCurrentSelection, getDiagnostics, getDefinition, diff tools (openDiff, openDiff3, openRevisionDiff, openCurrentChanges, applyPatch), runCommand
- Implements resources: buffer content, project info, diagnostics
- Per-project WebSocket connections for session isolation

### Setup
Claude Code needs to be configured to use the MCP server:
```bash
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": ["/path/to/claude-code-emacs/mcp-server/dist/index.js"]
}'
```

### Logging
The MCP server logs to a file for debugging purposes:
- Log file location: `.claude-code-emacs-mcp.log` in project root
- Logs include timestamps, connection status, and request/response details
- Useful for troubleshooting MCP integration issues
- Previous log location (`/tmp/claude-code-emacs-mcp.log`) is no longer used

### Development
When working on the MCP server:
1. Build with `make mcp-build`
2. Check logs for debugging: `tail -f .claude-code-emacs-mcp.log`
3. Test TypeScript code: `npm test --prefix mcp-server`
4. The server auto-starts when Claude Code requests MCP tools
5. For development with hot-reload: `make mcp-dev`

### MCP Communication Flow
1. Claude Code connects to MCP server via stdio
2. MCP server establishes WebSocket connection to Emacs (dynamic port)
3. MCP tools translate requests into Emacs Lisp commands
4. Results are sent back through the same chain

### Connection Health Monitoring
The MCP connection includes automatic health monitoring:
- **Ping/Pong mechanism**: Emacs sends ping messages every 30 seconds (configurable via `claude-code-emacs-mcp-ping-interval`)
- **Timeout detection**: If no pong is received within 10 seconds (configurable via `claude-code-emacs-mcp-ping-timeout`), the connection is considered lost
- **Session persistence**: Each project maintains its own WebSocket connection for isolation

### Known Issues and Solutions
- **WebSocket 400 error**: Fixed by ensuring the WebSocket URL includes a leading slash (e.g., `ws://localhost:port/?session=...`)
- **Connection timing**: WebSocket connections are established asynchronously; the `on-open` callback is used to ensure proper initialization
- **Timer management**: Ping timers are properly cleaned up on disconnect to prevent resource leaks
