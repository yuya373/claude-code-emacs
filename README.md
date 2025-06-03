# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

An Emacs package to run Claude Code within Emacs.

## Features

### MCP (Model Context Protocol) Integration
Claude Code Emacs includes MCP server integration, allowing Claude Code to interact directly with your Emacs environment.

#### Available MCP Tools
- **Open File**: Open any project file with optional text selection
- **Get Open Buffers**: List all open buffers in the current project
- **Get Current Selection**: Retrieve the currently selected text in Emacs
- **Get Diagnostics**: Access LSP diagnostics for project files

The MCP server runs on port 8766 by default and provides a WebSocket bridge for communication between Claude Code and Emacs.

## Starting and Closing Claude Code
- `claude-code-emacs-run` - Start Claude Code in the current project
- `claude-code-emacs-close` - Close the window displaying Claude Code buffer
- `claude-code-emacs-quit` - Quit Claude Code session and kill the buffer

## Prompt Management Features
A `.claude-code-emacs.prompt.md` file is created in each project root.
The buffer is opened with `switch-to-buffer-other-window`.

### Key Bindings
- `C-c C-s`: Send the markdown section at point to Claude Code buffer
- `C-c C-r`: Send the selected region to Claude Code buffer
- `C-c C-o`: Open Claude Code session
- `C-c C-i`: Insert a project file path with @ prefix (interactive selection)
- `C-c C-a`: Insert all open buffer file paths
- `C-c C-t`: Show transient menu for prompt buffer
- `@`: Typing @ automatically triggers file completion

## Transient Menus
### Main Menu
Display the main menu with `M-x claude-code-emacs-transient`

For detailed documentation about Claude Code slash commands, see the [official Claude Code documentation](https://docs.anthropic.com/en/docs/claude-code/cli-usage#slash-commands).

#### Session
- `c`: Run Claude Code
- `b`: Switch to Claude Code buffer
- `q`: Close Claude Code window
- `Q`: Quit Claude Code session
- `p`: Open Prompt File
- `s`: Send Region

#### Quick Send Keys
- `1` or `y`: Send "1" (useful for "yes" responses)
- `2`: Send "2"
- `3`: Send "3"
- `g`: Send "commit"
- `e`: Send Escape
- `m`: Send Return
- `r`: Send Ctrl+R (toggle expand)

#### Commands
- `i`: /init
- `k`: /clear
- `h`: /help
- `x`: Execute custom project command
- `X`: Execute global command (/user:)

#### Memory & Config
- `M`: /memory
- `C`: /config
- `o`: /compact

#### Review
- `R`: /review
- `P`: /pr_comments

#### Info & Account
- `$`: /cost
- `S`: /status
- `l`: /login
- `L`: /logout
- `B`: /bug
- `D`: /doctor

### Prompt Buffer Menu
In prompt buffers, display the menu with `C-c C-t` or `M-x claude-code-emacs-prompt-transient`

## Custom Commands
Claude Code Emacs supports custom commands that can be stored as markdown files and executed through the interface.

### Project-specific Commands
Store commands in `.claude/commands/*.md` within your project directory. These can be executed via `/project:command-name`.

- Create markdown files in `.claude/commands/` directory
- Use `$ARGUMENTS` as placeholders for user input
- Execute with `M-x claude-code-emacs-execute-custom-command` or `x` in the transient menu
- Example: `.claude/commands/deploy.md` can be run as `/project:deploy`

### User-specific Commands
Store global commands in `~/.claude/commands/*.md`. These can be executed via `/user:command-name`.

- Create markdown files in `~/.claude/commands/` directory
- Use `$ARGUMENTS` as placeholders for user input
- Execute with `M-x claude-code-emacs-execute-global-command` or `X` in the transient menu
- Example: `~/.claude/commands/format.md` can be run as `/user:format`

### Using Arguments
When a command contains `$ARGUMENTS` placeholders, you'll be prompted to provide values for each occurrence.

## Other Features
### Send Region
`claude-code-emacs-send-region` - Send the selected region or entire buffer to Claude Code

## Testing
### Running Tests
```bash
# Using Makefile
make test

# Or run directly
emacs -batch -l run-tests.el
```

### All Tasks (clean, compile, test)
```bash
make all
```

## Dependencies
- `projectile` - For project root detection
- `vterm` - For terminal emulation
- `transient` - For menu system
- `markdown-mode` - Base mode for prompt files
- `websocket` - For MCP server communication
- `lsp-mode` (optional) - For diagnostic information

## Installation
Clone this repository and add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)

;; Optional: Enable MCP integration
(require 'claude-code-emacs-mcp)

;; Optional: Set global keybinding for the main menu
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

### MCP Server Setup
The MCP server requires Node.js. To install dependencies and build:

```bash
# Install all dependencies (including MCP server)
make install-deps

# Build MCP server
make mcp-build
```

## Usage
1. Run `M-x claude-code-emacs-run` to start Claude Code in the current project
2. Use `M-x claude-code-emacs-open-prompt-file` to create/edit project-specific prompts
3. Access all commands through the transient menu with `C-c c`
4. Add Emacs as an MCP server with `claude mcp add-json ...`

### Adding Emacs as an MCP Server to Claude Code
```shell
claude mcp add-json emacs '{
  "type": "stdio",
  "command": "node",
  "args": [
    "/path/to/claude-code-emacs/mcp-server/dist/index.js"
  ]
}'
```

## License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
