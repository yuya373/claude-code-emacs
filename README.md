# Claude Code Emacs

[![CI Tests](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml/badge.svg)](https://github.com/yuya373/claude-code-emacs/actions/workflows/test.yml)

Atn Emacs package to run Claude Code CLI within Emacs. This package provides seamless integration with Claude Code, allowing you to run AI-powered coding sessions directly in your Emacs environment.

## Features

- **Project-specific sessions**: Each project gets its own isolated Claude Code session
- **Seamless buffer management**: Automatic buffer creation and switching
- **Smart file completion**: Type `@` to quickly reference project files
- **Custom commands**: Support for project-specific and global commands
- **Transient menus**: Intuitive menu system for all operations
- **Prompt management**: Dedicated mode for managing Claude Code prompts
- **MCP integration**: Direct interaction between Claude Code and Emacs

### MCP (Model Context Protocol) Integration
Claude Code Emacs includes MCP server integration, allowing Claude Code to interact directly with your Emacs environment.

**Important**: The MCP features require proper setup to work:
1. Configure Claude Code to use the MCP server (see "Adding Emacs as an MCP Server" section below)
2. Once configured, Claude Code automatically starts the MCP server when you begin a session
3. The MCP server will automatically connect to Emacs by calling `claude-code-emacs-mcp-register-port`
4. Without this setup, MCP tools and event notifications will not function

#### Available MCP Tools
- **Get Open Buffers**: List all open buffers in the current project
- **Get Current Selection**: Retrieve the currently selected text in Emacs
- **Get Diagnostics**: Get project-wide LSP diagnostics (requires specifying a buffer for LSP context)
- **Get Definition**: Find symbol definitions using LSP with preview
- **Find References**: Find all references to a symbol using LSP
- **Describe Symbol**: Get documentation and information about symbols using LSP hover
- **Diff Tools**: Powerful file comparison tools:
  - **openDiff**: Compare two files or buffers
  - **openDiff3**: Three-way file comparison for merge conflicts
  - **openRevisionDiff**: Compare file with any git revision
  - **openCurrentChanges**: Show uncommitted changes in ediff
  - **applyPatch**: Apply patch files using ediff
- **Send Notification**: Send notifications to the user via Emacs alert system
  - Uses `alert` package if available, falls back to `message` function
  - Useful for notifying task completion or errors
  - Always uses `claude-code` category for consistent alert handling

#### Real-time Event Notifications
The MCP server now sends real-time notifications to Claude Code about Emacs state changes:
- **Buffer List Updates**: Notifies when buffers are opened, closed, or modified
- **Buffer Content Changes**: Tracks file modifications with line-level granularity (batched per project)
- **Diagnostics Updates**: Sends LSP diagnostics changes as they occur (batched per project)

These events are automatically enabled and use efficient debouncing to minimize overhead. Changes are batched by project for optimal performance.

The MCP server uses dynamic port allocation for each project and provides a WebSocket bridge for communication between Claude Code and Emacs.

## Starting and Closing Claude Code
- `claude-code-emacs-run` - Start Claude Code in the current project
  - Use `C-u M-x claude-code-emacs-run` to interactively select options:
    - `--verbose`: Enable verbose logging
    - `--model sonnet` or `--model opus`: Select AI model
    - `--resume`: Resume last conversation
    - `--continue`: Continue from specific chat UUID
    - `--dangerously-skip-permissions`: Skip permission checks
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
- `f`: Fix LSP Diagnostic (requires lsp-mode)

#### Actions
- `s`: Send menu - Access send-related functions
- `i`: Insert menu - Access insert-related functions
- `/`: Slash commands menu - Access all Claude Code slash commands

#### Quick Send Keys
- `1` or `y`: Send "1" (useful for "yes" responses)
- `2`: Send "2"
- `3`: Send "3"
- `g`: Send "commit"
- `e`: Send Escape
- `m`: Send Return
- `r`: Send Ctrl+R (toggle expand)
- `TAB`: Send Shift+Tab (toggle auto accept)

#### Commands
- `x`: Execute custom project command
- `X`: Execute global command (/user:)

#### Git & GitHub
- `g`: Git & GitHub menu - Access git-related commands

### Send Menu
Access with `s` from main menu or `M-x claude-code-emacs-send-transient`
- `s`: Send text to Claude Code
- `r`: Send region to Claude Code

### Insert Menu
Access with `i` from main menu or `M-x claude-code-emacs-insert-transient`
- `r`: Insert region path with content to prompt buffer
- `i`: Insert current file path (@-prefixed) to prompt buffer

### Slash Commands Menu
Access with `/` from main menu or `M-x claude-code-emacs-slash-commands-transient`
Contains all Claude Code slash commands organized by category:
- Project & Session: `/init`, `/clear`, `/help`
- Memory & Config: `/memory`, `/config`, `/compact`
- Info & Status: `/cost`, `/status`
- Account: `/login`, `/logout`
- Other: `/bug`, `/doctor`

### Git & GitHub Menu
Access with `g` from main menu or `M-x claude-code-emacs-git-menu-transient`
- `g`: Send commit
- `p`: Send push
- `r`: Review
- `c`: PR comments

### Prompt Buffer Menu
In prompt buffers, display the menu with `C-c C-t` or `M-x claude-code-emacs-prompt-transient`

## Custom Commands
Claude Code Emacs supports custom commands that can be stored as markdown files and executed through the interface.

### Project-specific Commands
Store commands in `.claude/commands/*.md` within your project directory. These are executed via `/project:command-name`.

- Create markdown files in `.claude/commands/` directory
- Use `$ARGUMENTS` as placeholders for user input
- Execute with `M-x claude-code-emacs-execute-custom-command` or `x` in the transient menu
- Example: `.claude/commands/deploy.md` is run as `/project:deploy`

### User-specific Commands
Store global commands in `~/.claude/commands/*.md`. These are executed via `/user:command-name`.

- Create markdown files in `~/.claude/commands/` directory
- Use `$ARGUMENTS` as placeholders for user input
- Execute with `M-x claude-code-emacs-execute-global-command` or `X` in the transient menu
- Example: `~/.claude/commands/format.md` is run as `/user:format`

### Using Arguments
When a command contains `$ARGUMENTS` placeholders, you'll be prompted to provide values for each occurrence.

## Other Features
### Send Region
`claude-code-emacs-send-region` - Send the selected region to Claude Code (error if no region selected)

### Fix LSP Diagnostic
`claude-code-emacs-fix-diagnostic` - Select an LSP diagnostic from your project and automatically send a fix request to Claude Code. This feature:
- Shows all diagnostics from the current project (errors, warnings, info, hints)
- Sorts diagnostics by severity (errors first)
- Formats the selected diagnostic with file path, line number, and message
- Sends a properly formatted fix request to Claude Code
- Requires `lsp-mode` to be active

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
- `alert` (optional) - For desktop notifications (falls back to `message` if not available)

## Installation

### Prerequisites
- Emacs 28.1 or later
- [Claude Code CLI](https://docs.anthropic.com/en/docs/claude-code) installed and configured
- Node.js v16+ (for MCP server)
- Required Emacs packages: `projectile`, `vterm`, `transient`, `markdown-mode`, `websocket`
- Optional: `lsp-mode` (for enhanced diagnostics)

### Basic Setup
Clone this repository and add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)

;; Optional: Set global keybinding for the main menu
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

Note: The package is now modularized. All modules are loaded automatically when you require `claude-code-emacs`.

### MCP Server Setup
The MCP server enables Claude Code to interact with your Emacs environment:

```bash
# Install all dependencies (including MCP server)
make install-deps

# Build MCP server
make mcp-build
```

For detailed MCP setup instructions, see [docs/MCP-SETUP.md](docs/MCP-SETUP.md).

## Quick Start

1. **Start Claude Code**: `M-x claude-code-emacs-run` (or `C-c c c` with recommended keybinding)
2. **Open transient menu**: `C-c c` to see all available commands
3. **Create prompts**: `M-x claude-code-emacs-open-prompt-file` to manage project prompts

### Adding Emacs as an MCP Server
To enable MCP integration, you must configure Claude Code to use the MCP server:

1. **Build the MCP server** (if not already built):
   ```shell
   cd /path/to/claude-code-emacs
   make mcp-build
   ```

2. **Configure Claude Code**:
   ```shell
   claude mcp add-json emacs '{
     "type": "stdio",
     "command": "node",
     "args": [
       "/path/to/claude-code-emacs/mcp-server/dist/index.js"
     ]
   }'
   ```
   Replace `/path/to/claude-code-emacs` with the actual path to this package.

3. **MCP server starts automatically**:
   - Start a Claude Code session in your project
   - Claude Code automatically starts the configured MCP server
   - The MCP server will connect to Emacs and call `claude-code-emacs-mcp-register-port`
   - You'll see a message in Emacs confirming the connection
   - MCP tools are immediately available without needing to run `/mcp`

**Note**: MCP features (tools, resources, and event notifications) will only work after completing these setup steps.

## Architecture

The package is organized into focused modules:

- **claude-code-emacs.el** - Main entry point, loads all modules
- **claude-code-emacs-core.el** - Core utilities (chunking, error handling, session management)
- **claude-code-emacs-buffer.el** - Buffer naming and management
- **claude-code-emacs-commands.el** - Command execution and slash commands
- **claude-code-emacs-ui.el** - Transient menu interfaces
- **claude-code-emacs-prompt.el** - Prompt file mode and operations
- **claude-code-emacs-mcp.el** - MCP WebSocket client integration
- **claude-code-emacs-mcp-connection.el** - WebSocket connection management with automatic reconnection
- **claude-code-emacs-mcp-protocol.el** - MCP protocol implementation
- **claude-code-emacs-mcp-tools.el** - MCP tool handlers (file operations, diagnostics, diff tools)
- **claude-code-emacs-mcp-events.el** - Real-time event notifications (buffer changes, diagnostics)

## Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass with `make test`
5. Submit a pull request

## MCP Server Features

### Connection Management
The MCP server maintains a stable WebSocket connection with automatic health monitoring:
- **Automatic port registration**: MCP server calls `claude-code-emacs-mcp-register-port` when started
- **Ping/Pong heartbeat**: Sends periodic ping messages (default 30s interval) to detect connection issues
- **Configurable timeouts**: Customize ping interval and timeout via `claude-code-emacs-mcp-ping-interval` and `claude-code-emacs-mcp-ping-timeout`
- **Per-project isolation**: Each project gets its own MCP connection

### Available MCP Tools
- **getOpenBuffers**: List all open buffers in the current project
- **getCurrentSelection**: Get currently selected text
- **getDiagnostics**: Get project-wide LSP diagnostics (requires buffer name for LSP context)
- **getDefinition**: Find symbol definitions using LSP with preview (shows 3 lines before/after)
- **findReferences**: Find all references to a symbol using LSP (requires `lsp-mode`)
- **describeSymbol**: Get symbol documentation using LSP hover, with Markdown formatting for code blocks
- **openDiff**: Compare two files using ediff
- **openDiff3**: Three-way file comparison
- **openRevisionDiff**: Compare file with git revision
- **openCurrentChanges**: Show uncommitted changes
- **applyPatch**: Apply patch files using ediff
- **openDiffContent**: Compare two text contents in temporary buffers without needing files
- **sendNotification**: Send desktop notifications to user via Emacs alert package

### Available MCP Resources
The MCP server exposes Emacs data as resources that Claude Code can access:
- **Buffer Resources** (`file://`): Access content of open buffers with unsaved changes
- **Project Resources** (`emacs://project/`): Project metadata and file listings
- **Diagnostics Resources** (`emacs://diagnostics/`): LSP diagnostics for buffers and projects

### Real-time Event Notifications
The MCP server now supports real-time notifications from Emacs to Claude Code:

#### Event Types
- **emacs/bufferListUpdated**: Notifies when buffers are opened, closed, or their states change
  - Tracks buffer paths, names, active/modified status
  - Filtered by project to avoid cross-project noise
  
- **emacs/bufferContentModified**: Notifies when buffer content changes
  - Includes file path, affected line ranges, and change length
  - Multiple changes are batched per project for efficiency
  - Changes to the same region are automatically merged
  
- **emacs/diagnosticsChanged**: Notifies when LSP diagnostics update
  - Includes all diagnostics for affected files
  - Batched by project to reduce notification overhead
  - Provides line, column, severity, and message for each diagnostic

#### Features
- **Automatic debouncing**: Events are debounced to avoid overwhelming Claude Code
  - Buffer changes: 0.5s delay (configurable via `claude-code-emacs-mcp-events-change-delay`)
  - Buffer list updates: 1.0s delay (configurable via `claude-code-emacs-mcp-events-buffer-list-delay`)
  - Diagnostics: 1.0s delay (configurable via `claude-code-emacs-mcp-events-diagnostics-delay`)
  
- **Project isolation**: Each project connection only receives events relevant to its files
- **Efficient batching**: Multiple changes are grouped into single notifications per project

#### Enabling Event Notifications
Event notifications are automatically enabled when the MCP connection is established. To manually control:

```elisp
;; Enable event notifications
(claude-code-emacs-mcp-events-enable)

;; Disable event notifications
(claude-code-emacs-mcp-events-disable)
```

### Planned Features
- **getWorkspaceFolders**: List all project folders
- **checkDocumentDirty**: Check for unsaved changes
- **saveDocument**: Save files with unsaved changes
- **getSymbols**: Access code symbols and definitions


## License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.
