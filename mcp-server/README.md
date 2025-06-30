# claude-code-emacs-mcp-server

MCP (Model Context Protocol) server that enables Claude Code to interact with Emacs. This package provides a bridge between Claude Code and your Emacs editor, allowing Claude to read files, navigate code, and use LSP features directly from Emacs.

## Features

### Tools
- **openFile**: Open files in Emacs with optional text selection
- **getOpenBuffers**: List all open buffers in the current project
- **getCurrentSelection**: Get the currently selected text in Emacs
- **getDiagnostics**: Retrieve LSP diagnostics for the project
- **getDefinition**: Find symbol definitions using LSP
- **findReferences**: Find all references to a symbol using LSP
- **runCommand**: Execute Emacs commands via emacsclient subprocess (with security restrictions)

### Diff Tools
- **openDiff**: Compare two files or buffers
- **openDiff3**: Three-way file comparison
- **openRevisionDiff**: Compare file with git revision
- **openCurrentChanges**: Show uncommitted changes
- **applyPatch**: Apply patch files using ediff
- **openDiffContent**: Compare two text contents in temporary buffers

### Other Tools
- **sendNotification**: Send notifications to Emacs (using the alert package)
- **describeSymbol**: Get documentation for symbols using LSP hover

### Resources
- Buffer content access
- Project information
- LSP diagnostics

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
Claude Code <--(stdio/JSON-RPC)--> MCP Server <--(WebSocket:8766)--> Emacs
```

- Claude Code communicates with the MCP server using stdio (JSON-RPC protocol)
- The MCP server maintains a WebSocket connection to Emacs on port 8766
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

The default WebSocket port is 8766. To use a different port:

1. In Emacs, set `claude-code-emacs-mcp-port`:
   ```elisp
   (setq claude-code-emacs-mcp-port 8767)
   ```

2. Pass the port as an argument in the MCP configuration:
   ```json
   "args": ["claude-code-emacs-mcp-server", "8767"]
   ```

## License

GPL-3.0-or-later

## Links

- [GitHub Repository](https://github.com/yuya373/claude-code-emacs)
- [Emacs Package Documentation](https://github.com/yuya373/claude-code-emacs#readme)
- [Issue Tracker](https://github.com/yuya373/claude-code-emacs/issues)
