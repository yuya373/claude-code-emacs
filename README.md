# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

Run [Claude Code](https://docs.anthropic.com/en/docs/claude-code) AI coding sessions directly in Emacs with powerful MCP integration.

## Quick Start

```elisp
;; Add to your init.el
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

```bash
# Install dependencies and build MCP server
make install-deps
make mcp-build

# Configure Claude Code to use MCP
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": ["/path/to/claude-code-emacs/mcp-server/dist/index.js"]
}'
```

Now run `M-x claude-code-emacs-run` to start a session!

## Key Features

### 🚀 Core Features
- **Project-isolated sessions** - Each project gets its own Claude Code buffer
- **Smart file completion** - Type `@` in prompts to reference project files
- **Transient menus** - Press `C-c c` for the main menu with all commands
- **Custom commands** - Define reusable commands in `.claude/commands/*.md`
- **Project prompts** - `.claude-code-emacs.prompt.md` file per project for persistent context

### 🔌 MCP Integration
Claude Code can directly interact with your Emacs environment:
- **Buffer operations** - List/read open buffers, get selected text
- **LSP integration** - Get diagnostics, find definitions/references, describe symbols
- **Diff tools** - Compare files, view git changes, apply patches
- **Real-time events** - Buffer changes and diagnostics sent to Claude Code automatically

### ⌨️ Key Bindings

| Key | Action |
|-----|--------|
| `C-c c` | Open main transient menu |
| `C-u M-x claude-code-emacs-run` | Start with options (model, resume, etc.) |
| `1`/`y` | Quick "yes" response |
| `TAB` | Toggle auto-accept |
| `C-c C-s` | Send section at point (in prompt buffer) |
| `@` | File completion (in prompt buffer) |

## Common Workflows

### Project Prompts
Each project gets a `.claude-code-emacs.prompt.md` file at the project root:
```markdown
# Project Context
This is a React app with TypeScript...

# Current Task
Implement user authentication

# Code Style
- Use functional components
- Prefer hooks over class components
```
Open with `M-x claude-code-emacs-open-prompt-file` or `p` in transient menu.

### Fix LSP Errors
```elisp
M-x claude-code-emacs-fix-diagnostic
;; or press 'f' in transient menu
```

### Custom Commands
Create `.claude/commands/refactor.md`:
```markdown
Refactor the following code: $ARGUMENTS
```
Execute with `x` in transient menu → select "refactor"

### Git Operations
Press `g` in main menu for git commands:
- `g` - commit
- `p` - push
- `r` - review changes
- `c` - PR comments

## Requirements

- Emacs 28.1+
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code) installed
- Node.js 16+ (for MCP)
- Packages: `projectile`, `vterm`, `transient`, `markdown-mode`, `websocket`
- Optional: `lsp-mode`, `alert`

## Installation Details

See [docs/MCP-SETUP.md](docs/MCP-SETUP.md) for detailed MCP configuration.

## Architecture

- **Modular design** - Separate modules for buffer management, commands, UI, MCP
- **Per-project WebSocket** - Each project maintains its own MCP connection
- **Automatic reconnection** - MCP connection health monitoring with ping/pong
- **Event batching** - Efficient real-time notifications with debouncing

## Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Run `make test` to ensure all tests pass
5. Submit a pull request

## License

GPL-3.0-or-later
