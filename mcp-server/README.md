# claude-code-emacs-mcp-server

MCP (Model Context Protocol) server that enables Claude Code to interact with Emacs. This package provides a bridge between Claude Code and your Emacs editor, allowing Claude to read files, navigate code, and use LSP features directly from Emacs.

## Features

### Tools
- **getOpenBuffers**: List all open buffers in the current project
- **getCurrentSelection**: Get the currently selected text in Emacs
- **getDiagnostics**: Retrieve LSP diagnostics for the project (requires buffer parameter)
- **getDefinition**: Find symbol definitions using LSP with preview
- **findReferences**: Find all references to a symbol using LSP
- **describeSymbol**: Get documentation for symbols using LSP hover
- **sendNotification**: Send notifications to Emacs (using the alert package)

### Diff Tools
- **openDiff**: Compare two files or buffers
- **openDiff3**: Three-way file comparison
- **openRevisionDiff**: Compare file with git revision
- **openCurrentChanges**: Show uncommitted changes
- **applyPatch**: Apply patch files using ediff
- **openDiffContent**: Compare two text contents in temporary buffers

### Real-time Events
- **emacs/bufferListUpdated**: Notifies when buffers are opened, closed, or modified
- **emacs/bufferContentModified**: Notifies when buffer content changes with line-level information
- **emacs/diagnosticsChanged**: Notifies when LSP diagnostics are updated

### Resources
- Buffer content access
- Project information

## Installation

```bash
npm install -g claude-code-emacs-mcp-server
```

Or install locally:

```bash
npm install claude-code-emacs-mcp-server
```

## Setup

1. Configure Claude Code to use this MCP server:
   
   For global installation:
   ```bash
   claude mcp add-json emacs '{
     "type": "stdio",
     "command": "claude-code-emacs-mcp"
   }'
   ```
   
   For local installation:
   ```bash
   claude mcp add-json emacs '{
     "type": "stdio",
     "command": "npx",
     "args": ["claude-code-emacs-mcp-server"]
   }'
   ```

2. Install and configure the Emacs package:
   - Install `claude-code-emacs` from MELPA or GitHub
   - The package will automatically handle the WebSocket connection

3. Start Claude Code and the MCP server will automatically start when Claude requests MCP tools

## Architecture

The MCP server acts as a bridge between Claude Code and Emacs:

```
Claude Code <--(stdio/JSON-RPC)--> MCP Server <--(WebSocket:dynamic port)--> Emacs
```

- Claude Code communicates with the MCP server using stdio (JSON-RPC protocol)
- The MCP server creates a WebSocket server on a dynamic port
- Port information is registered with Emacs via emacsclient
- Each project maintains its own isolated WebSocket connection
- Emacs handles the actual file operations and provides editor state
- The server automatically starts when Claude Code needs to use Emacs tools

## Requirements

- Node.js 16 or higher
- Emacs 28.1 or higher with `claude-code-emacs` package
- Claude Code CLI

## Development

To contribute or modify:

```bash
# Clone the repository
git clone https://github.com/yuya373/claude-code-emacs.git
cd claude-code-emacs/mcp-server

# Install dependencies
npm install

# Run in development mode with auto-rebuild
npm run dev

# Run tests
npm test

# Build
npm run build
```

## Debugging

The MCP server logs to a file for troubleshooting:
- Log file: `.claude-code-emacs-mcp.log` in project root
- View logs: `tail -f .claude-code-emacs-mcp.log`
- Logs include:
  - Server startup and connection events
  - Emacs WebSocket connection status
  - Request/response debugging information
  - Error messages and stack traces

## Port Configuration

The MCP server uses dynamic port allocation for the WebSocket connection. When the server starts:

1. It automatically finds an available port
2. Registers the port with Emacs via `emacsclient`
3. Emacs connects to the registered port

This eliminates port conflicts and allows multiple projects to run simultaneously. No manual port configuration is needed.

## License

GPL-3.0-or-later

## Links

- [GitHub Repository](https://github.com/yuya373/claude-code-emacs)
- [Emacs Package Documentation](https://github.com/yuya373/claude-code-emacs#readme)
- [Issue Tracker](https://github.com/yuya373/claude-code-emacs/issues)
