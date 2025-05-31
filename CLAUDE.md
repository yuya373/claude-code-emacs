# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs package that provides integration with Claude Code CLI. The package allows running Claude Code sessions within Emacs using vterm mode.

## Development Commands

### Testing
```bash
# Run all tests
make test

# Run tests directly
emacs -batch -l run-tests.el

# Run a specific test
emacs -batch -l run-tests.el -f ert-run-tests-batch-and-exit 'test-name-pattern'
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
```

### CI/CD
- Tests run automatically on push/PR via GitHub Actions
- Tests against Emacs 28.1, 29.1, and snapshot versions
- Special handling for vterm module compilation in CI environment

## Architecture

The package implements a complete Claude Code integration by:

1. **Session Management**: Each project gets its own Claude Code buffer named `*claude:<project-root>*`. This allows multiple concurrent sessions across different projects.

2. **Prompt File System**: Creates `.claude-code-emacs.prompt.md` files in each project root. These serve as persistent context that can be sent to Claude Code in chunks.

3. **String Chunking**: Long strings are split into 50-character chunks before sending to avoid terminal input limitations. This is handled transparently by `claude-code-emacs-chunk-string`.

4. **Two-Layer Interface**:
   - Direct commands via `claude-code-emacs-send-string` for programmatic interaction
   - Transient menus for user-friendly access to all functionality

5. **Mode Integration**: Custom major modes for both vterm sessions (`claude-code-emacs-mode`) and prompt files (`claude-code-emacs-prompt-mode`)

## Key Functions

### Session Management
- `claude-code-emacs-run`: Starts Claude Code CLI in a project-specific buffer
- `claude-code-emacs-switch-to-buffer`: Switch to existing Claude Code buffer for current project
- Buffer naming follows pattern `*claude:<project-root>*`

### Slash Commands
All slash commands have been implemented and use `claude-code-emacs-send-string` for consistency:
- `claude-code-emacs-init`: Sends `/init` command
- `claude-code-emacs-clear`: Sends `/clear` command
- `claude-code-emacs-help`: Sends `/help` command
- `claude-code-emacs-config`: Sends `/config` command with optional arguments
- `claude-code-emacs-memory`: Sends `/memory` command
- `claude-code-emacs-compact`: Sends `/compact` command with optional instructions
- `claude-code-emacs-cost`: Sends `/cost` command
- `claude-code-emacs-status`: Sends `/status` command
- `claude-code-emacs-review`: Sends `/review` command
- `claude-code-emacs-pr-comments`: Sends `/pr_comments` command
- `claude-code-emacs-bug`: Sends `/bug` command
- `claude-code-emacs-doctor`: Sends `/doctor` command
- `claude-code-emacs-login`: Sends `/login` command
- `claude-code-emacs-logout`: Sends `/logout` command

### Prompt Management
- `claude-code-emacs-open-prompt-file`: Creates/opens project-specific prompt file
- `claude-code-emacs-prompt-mode`: Markdown-based major mode for prompt files
- `claude-code-emacs-send-prompt-at-point`: Send markdown section at point to Claude Code
- `claude-code-emacs-send-prompt-region`: Send selected region to Claude Code
- LSP mode integration: Automatically configures language ID as "markdown" when lsp-mode is available

### Quick Send Functions
- `claude-code-emacs-send-1`: Send "1" to Claude Code
- `claude-code-emacs-send-2`: Send "2" to Claude Code
- `claude-code-emacs-send-3`: Send "3" to Claude Code
- `claude-code-emacs-send-commit`: Send "commit" to Claude Code
- `claude-code-emacs-send-escape`: Send ESC key to Claude Code
- `claude-code-emacs-send-return`: Send Return key to Claude Code

### Utility Functions
- `claude-code-emacs-send-string`: Core function for sending strings to Claude Code buffer
- `claude-code-emacs-send-region`: Send selected region or entire buffer
- `claude-code-emacs-chunk-string`: Splits long strings into manageable chunks

## Transient Menus

### Main Menu (`claude-code-emacs-transient`)
Organized into logical groups:
- Session: Run, switch buffer, open prompt file, send region
- Quick Send: Number keys (1/y, 2, 3), commit (g), escape (e), return (m)
- Commands: Init, clear, help
- Memory & Config: Memory, config, compact
- Review: Review code, PR comments
- Info: Cost, status
- Account: Login, logout
- Other: Bug report, doctor

Note: The "1" key can also be triggered with "y" for intuitive "yes" responses.

### Prompt Mode Menu (`claude-code-emacs-prompt-transient`)
Available in prompt buffers with `C-c C-t`:
- Send section at point
- Send region
- Run/switch to Claude Code

## Dependencies

- projectile: For project root detection
- vterm: Terminal emulation mode
- transient: Menu system
- markdown-mode: Base for prompt file editing
- lsp-mode (optional): For LSP integration

## Testing Strategy

The test suite (`test-claude-code-emacs.el`) uses mock implementations to avoid requiring actual vterm instances. Key testing patterns:

1. **Mock vterm buffers**: Tests create fake buffers that simulate vterm behavior
2. **Process simulation**: Mock process objects for testing send functions
3. **Integration tests**: Verify complete workflows like prompt file creation and sending

## Package Structure

```
claude-code-emacs/
├── claude-code-emacs.el      # Main implementation
├── test-claude-code-emacs.el  # Test suite
├── run-tests.el              # Test runner
├── install-deps.el           # Dependency installer
├── Makefile                  # Build automation
├── README.md                 # English documentation
├── README.ja.md              # Japanese documentation
└── CLAUDE.md                 # This file
```
