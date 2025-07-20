# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

Run [Claude Code](https://docs.anthropic.com/en/docs/claude-code) AI coding sessions directly in Emacs with powerful MCP integration.

## Quick Start

```elisp
;; Add to your init.el
(add-to-list 'load-path "/path/to/claude-code")
(require 'claude-code)
(global-set-key (kbd "C-c c") 'claude-code-transient)
```

```bash
# Install MCP server globally
npm install -g claude-code-mcp-server

# Configure Claude Code to use MCP
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "claude-code-mcp"
}'
```

Now run `M-x claude-code-run` to start a session!

## Key Features

### 🚀 Core Features
- **Project-isolated sessions** - Each project gets its own Claude Code buffer
- **Smart file completion** - Type `@` in prompts to reference project files
- **Transient menus** - Press `C-c c` for the main menu with all commands
- **Custom commands** - Define reusable commands in `.claude/commands/*.md`
- **Project prompts** - `.claude-code.prompt.md` file per project for persistent context

### 🔌 MCP Integration
Claude Code can directly interact with your Emacs environment:
- **Buffer operations** - List/read open buffers, get selected text
- **LSP integration** - Get diagnostics, find definitions/references, describe symbols
- **Diff tools** - Compare files, view git changes, apply patches
- **Real-time events** - Buffer changes and diagnostics sent to Claude Code automatically

### ⌨️ Key Bindings

#### Global
| Key | Action |
|-----|--------|
| `C-c c` | Open main transient menu |
| `C-u M-x claude-code-run` | Start with options (model, resume, etc.) |

#### In Claude Code Session (vterm mode)
| Key | Action |
|-----|--------|
| `C-c C-q` | Close Claude Code window |
| `C-c C-k` | Send Escape key |
| `C-c C-r` | Send Ctrl+R (toggle expand) |
| `C-c C-e` | Send Ctrl+E (toggle expand more) |
| `C-c RET` | Send Return key |
| `C-c TAB` | Send Shift+Tab (toggle auto-accept) |
| `C-c C-t` | Open transient menu |

#### In Prompt Buffer
| Key | Action |
|-----|--------|
| `C-c C-s` | Send section at point |
| `C-c C-r` | Send selected region |
| `C-c C-o` | Open Claude Code session |
| `C-c C-t` | Open prompt transient menu |
| `@` | File completion |

#### In Transient Menu (C-c c)
| Key | Action |
|-----|--------|
| `1`/`y` | Send 1 (yes) |
| `2` | Send 2 |
| `3` | Send 3 |
| `k` | Send Escape |
| `e` | Send Ctrl+E (toggle expand more) |
| `r` | Send Ctrl+R (toggle expand) |
| `g` | Git & GitHub menu |
| `/` | Slash commands menu |

## Common Workflows

### Project Prompts
Each project gets a `.claude-code.prompt.md` file at the project root. When you open this file, it automatically positions at the end for quick prompt entry:
```markdown
# Claude Code Prompts for my-project

This file contains prompts for Claude Code sessions.

## Example Prompts

Fix the bug in @src/utils.js where the parser fails on empty strings

---

Add unit tests for @src/api/auth.js
```
Open with `M-x claude-code-open-prompt-file` or `p` in transient menu.

### Fix LSP Errors
```elisp
M-x claude-code-fix-diagnostic
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
- Required packages: `projectile`, `vterm`, `transient`, `markdown-mode`
- Optional packages:
  - `lsp-mode` (9.0.0+): For LSP diagnostic fixing and MCP tools integration
  - `websocket` (1.15+): For MCP server WebSocket communication
  - `alert`: For desktop notifications

## Installation Details

### MCP Server Installation

#### Option 1: Global Installation (Recommended)
```bash
# Install globally from npm
npm install -g claude-code-mcp-server

# Configure Claude Code
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "claude-code-mcp"
}'
```

#### Option 2: Using npx (No Installation Required)
```bash
# Configure Claude Code to use npx
# npx will download and run the package on demand
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "npx",
  "args": ["claude-code-mcp-server"]
}'
```

#### Option 3: Build from Source
```bash
# If you cloned the repository
cd /path/to/claude-code/mcp-server
npm install
npm run build

# Configure Claude Code
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": ["/path/to/claude-code/mcp-server/dist/index.js"]
}'
```

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

### Development Commands

```bash
# Compile with all dependencies (including optional ones)
make compile

# Compile without optional dependencies (websocket, lsp-mode)
# Useful for testing MELPA compatibility
make compile-minimal

# Run tests
make test

# Run package-lint
make lint
```

## Release Process

Releases are automated using GitHub Actions:

### Creating a New Release

#### Option 1: Using GitHub CLI (Recommended)
```bash
# Create a draft release
./scripts/create-release.sh 0.2.0

# Or create and publish immediately
./scripts/create-release.sh 0.2.0 --publish
```

#### Option 2: Using GitHub Actions
1. Go to Actions → Create Release Draft → Run workflow
2. Enter version number (e.g., 0.2.0)

#### Final Steps
1. Review the auto-generated release notes
2. Edit if needed
3. Click "Publish release" (if using draft)

3. **Automated Steps** (After publishing):
   - Git tag is created automatically
   - Version numbers are updated in:
     - `claude-code.el`
     - `mcp-server/package.json`
   - MCP server is published to npm
   - MELPA recipe is generated

### Manual Version Update

For local version updates:
```bash
./scripts/update-version.sh 0.2.0
```

## License

GPL-3.0-or-later
