# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs package that provides integration with Claude Code CLI. The package allows running Claude Code sessions within Emacs using vterm mode, with each project getting its own isolated session.

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

## Architecture

### Session Management
Each project gets its own Claude Code buffer named `*claude:<project-root>*`. Key components:
- `claude-code-emacs-buffer-name()` - Generates unique buffer names per project
- `claude-code-emacs-ensure-buffer()` - Ensures buffer exists before operations
- `claude-code-emacs-with-vterm-buffer` - Helper macro for buffer context operations

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
   
2. **Global Commands** (`~/.claude/commands/*.md`)
   - Sent as `/user:command-name`
   - Functions: `claude-code-emacs-execute-global-command`

Both support `$ARGUMENTS` placeholders with interactive prompting.

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
- **`claude-code-emacs-vterm-mode`** - For Claude Code sessions
- **`claude-code-emacs-prompt-mode`** - For prompt markdown files
- Both have custom keymaps and integrate with their parent modes

### Transient Menu System
- Main menu: `claude-code-emacs-transient`
- Prompt menu: `claude-code-emacs-prompt-transient`
- Organized into logical groups with mnemonic key bindings

## Testing Strategy

Tests use mock implementations to avoid vterm dependencies:
- Mock vterm functions with `cl-letf`
- Test data flows rather than terminal interactions
- Integration tests verify complete workflows

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

### Dependencies
- **Required**: projectile, vterm, transient, markdown-mode, websocket
- **Optional**: lsp-mode (for diagnostics and language ID configuration)

When modifying this package:
1. Add tests for new functionality
2. Use the established macro patterns for new commands
3. Maintain project isolation in buffer naming
4. Follow the chunking pattern for long strings

## MCP Server

### Architecture
The MCP server provides a bridge between Claude Code and Emacs:
- WebSocket server on port 8766 for Emacs connection
- stdio interface for Claude Code MCP protocol
- Implements tools: openFile, getOpenBuffers, getCurrentSelection, getDiagnostics

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
- Log file location: `/tmp/claude-code-emacs-mcp.log`
- Logs include timestamps and connection status
- Useful for troubleshooting MCP integration issues

### Development
When working on the MCP server:
1. Build with `make mcp-build`
2. Check logs for debugging: `tail -f /tmp/claude-code-emacs-mcp.log`
3. Test TypeScript code: `npm test --prefix mcp-server`
4. The server auto-starts when Claude Code requests MCP tools