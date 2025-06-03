# MCP Server Setup Guide

This guide explains how to set up and use the MCP (Model Context Protocol) integration between Claude Code and Emacs.

## Prerequisites

- Node.js (v16 or later)
- Emacs with claude-code-emacs package
- Claude Code CLI

## Installation

1. **Build the MCP server**:
   ```bash
   cd /path/to/claude-code-emacs
   make mcp-build
   ```

2. **Configure Claude Code**:
   
   Create a `.mcp.json` file in your project root:
   
   ```json
   {
     "mcpServers": {
       "emacs": {
         "type": "stdio",
         "command": "node",
         "args": [
           "/absolute/path/to/claude-code-emacs/mcp-server/dist/index.js",
           "8766"
         ]
       }
     }
   }
   ```
   
   Note: The port number (8766) must match the `claude-code-emacs-mcp-port` setting in Emacs.

3. **Load the Emacs package**:
   
   In your Emacs configuration:
   ```elisp
   (add-to-list 'load-path "/path/to/claude-code-emacs")
   (require 'claude-code-emacs)
   (require 'claude-code-emacs-mcp)  ; MCP integration
   ```

## Usage

1. Start Emacs and open a project
2. Run `M-x claude-code-emacs-run` to start Claude Code
3. Claude Code will automatically start the MCP server via .mcp.json
4. Emacs will automatically connect to the MCP server WebSocket

## Available MCP Tools

Claude Code can now use these tools to interact with your Emacs session:

### `openFile`
Opens a file in Emacs with optional text selection.
- Parameters:
  - `path`: File path relative to project root
  - `startText`: (optional) Text to start selection from
  - `endText`: (optional) Text to end selection at

### `getOpenBuffers`
Lists all open buffers in the current project.
- Parameters:
  - `includeHidden`: (optional) Include hidden buffers

### `getCurrentSelection`
Gets the currently selected text in Emacs.
- No parameters required

### `getDiagnostics`
Retrieves LSP diagnostics for the project.
- Parameters:
  - `bufferPath`: (optional) Specific buffer path, or all project buffers

## Troubleshooting

### MCP server not starting
- Run `claude mcp` to check MCP server status
- Use `claude --mcp-debug` to see detailed error logs
- Ensure the port (default: 8766) is not in use
- Verify Node.js is installed and accessible

### Connection issues
- Make sure the MCP server path in Claude Code config is absolute
- Check that the Emacs package is loaded correctly
- Verify `lsp-mode` is installed for diagnostics functionality

### Testing the connection
1. Start Claude Code: `M-x claude-code-emacs-run`
2. Check the `*Messages*` buffer for "Connected to MCP server WebSocket on port 8766"
3. In Claude Code, try using a tool like: "Show me all open buffers"
4. To manually reconnect: `M-x claude-code-emacs-mcp-connect`

## Development

To rebuild the MCP server after changes:
```bash
make mcp-clean
make mcp-build
```

To run in development mode:
```bash
cd mcp-server
npm run dev
```