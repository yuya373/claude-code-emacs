# Claude Code Emacs MCP Server

This MCP (Model Context Protocol) server enables Claude Code to interact with Emacs.

## Features

- **openFile**: Open files in Emacs with optional text selection
- **getOpenBuffers**: List all open buffers in the current project
- **getCurrentSelection**: Get the currently selected text in Emacs
- **getDiagnostics**: Retrieve LSP diagnostics for the project

## Setup

1. Build the MCP server:
   ```bash
   cd mcp-server
   npm install
   npm run build
   ```

2. Configure Claude Code to use this MCP server:
   - Copy `claude_desktop_config.json` to your Claude Code configuration directory
   - Or add the following to your existing config:
   ```json
   {
     "mcpServers": {
       "emacs": {
         "command": "node",
         "args": ["path/to/mcp-server/dist/index.js"]
       }
     }
   }
   ```

3. Start Emacs with the claude-code-emacs package loaded

4. Run Claude Code - the MCP server will start automatically when needed

## Architecture

The MCP server acts as a bridge between Claude Code and Emacs:

```
Claude Code <-> MCP Server <-> WebSocket <-> Emacs
```

- Claude Code communicates with the MCP server using stdio (JSON-RPC)
- The MCP server maintains a WebSocket connection to Emacs
- Emacs handles the actual file operations and provides editor state

## Development

```bash
# Run in development mode with auto-rebuild
npm run dev

# Run tests
npm test

# Clean build artifacts
npm run clean
```