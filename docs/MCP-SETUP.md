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
   
   Use the CLI wizard
   ```shell
   claude mcp add-json emacs '{
     "type": "stdio",
     "command": "node",
     "args": [
       "/path/to/claude-code-emacs/mcp-server/dist/index.js"
     ]
   }'
   ```
   
   Note: Replace `/path/to/claude-code-emacs` with the actual path

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
3. Type `/mcp` in the Claude Code session to enable MCP integration
4. The MCP server will start and Emacs will automatically connect

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
- Check MCP server logs: `tail -f /tmp/claude-code-emacs-mcp.log`
- The MCP server uses dynamic port allocation for each project
- Verify Node.js is installed and accessible: `node --version`
- Check if the MCP server is running: `ps aux | grep mcp-server`
- Ensure the MCP server is built: `make mcp-build`

### Connection issues
- Make sure the MCP server path in Claude Code config is absolute
- Check that the Emacs package is loaded correctly
- Verify `lsp-mode` is installed for diagnostics functionality (optional)
- Check port info file: `ls /tmp/claude-code-emacs-mcp-*.port`

### Testing the connection
1. Start Claude Code: `M-x claude-code-emacs-run`
2. Type `/mcp` to start the MCP server
3. Check the `*Messages*` buffer for "MCP WebSocket opened for project"
4. In Claude Code, try using a tool like: "Show me all open buffers"
5. To manually reconnect: `M-x claude-code-emacs-mcp-maybe-ensure-connection`

### Common issues and solutions
- **"WebSocket 400 error"**: Fixed in latest version - ensure WebSocket URL has leading slash
- **"MCP tools not available"**: Make sure you typed `/mcp` in Claude Code
- **"Cannot find module"**: Rebuild the MCP server with `make mcp-build`
- **"Connection timing out"**: The server uses dynamic ports - check logs for the actual port
- **"Permission denied"**: Check file permissions on the MCP server directory

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
