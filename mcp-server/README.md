# Claude Code Emacs MCP Server

This MCP (Model Context Protocol) server enables Claude Code to interact with Emacs.

## Features

### Tools
- **openFile**: Open files in Emacs with optional text selection
- **getOpenBuffers**: List all open buffers in the current project
- **getCurrentSelection**: Get the currently selected text in Emacs
- **getDiagnostics**: Retrieve LSP diagnostics for the project
- **getDefinition**: Find symbol definitions using LSP
- **runCommand**: Execute Emacs commands via emacsclient subprocess (with security restrictions)

### Diff Tools
- **openDiff**: Compare two files or buffers
- **openDiff3**: Three-way file comparison
- **openRevisionDiff**: Compare file with git revision
- **openCurrentChanges**: Show uncommitted changes
- **applyPatch**: Apply patch files using ediff

### Resources
- Buffer content access
- Project information
- LSP diagnostics

## Setup

1. Build the MCP server:
   ```bash
   cd mcp-server
   npm install
   npm run build
   ```

2. Configure Claude Code to use this MCP server:
   
   Method 1: Use the CLI wizard
   ```bash
   claude mcp add-json emacs '{
     "type": "stdio",
     "command": "node",
     "args": ["/absolute/path/to/claude-code-emacs/mcp-server/dist/index.js"]
   }'
   ```
   
   Note: Replace `/absolute/path/to/claude-code-emacs` with the actual path

3. Start Emacs with the claude-code-emacs package loaded

4. Start Claude Code and the MCP server will automatically start when Claude requests MCP tools

## Architecture

The MCP server acts as a bridge between Claude Code and Emacs:

```
Claude Code <--(stdio/JSON-RPC)--> MCP Server <--(WebSocket:8766)--> Emacs
```

- Claude Code communicates with the MCP server using stdio (JSON-RPC protocol)
- The MCP server maintains a WebSocket connection to Emacs on port 8766
- Emacs handles the actual file operations and provides editor state
- The server automatically starts when Claude Code needs to use Emacs tools

## Development

```bash
# Run in development mode with auto-rebuild
npm run dev

# Run tests
npm test

# Clean build artifacts
npm run clean
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
   "args": ["/path/to/mcp-server/dist/index.js", "8767"]
   ```
