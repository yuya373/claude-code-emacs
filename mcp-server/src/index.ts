import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ListToolsRequestSchema } from '@modelcontextprotocol/sdk/types.js';
import { EmacsBridge } from './emacs-bridge.js';
import {
  handleOpenFile,
  handleGetOpenBuffers,
  handleGetCurrentSelection,
  handleGetDiagnostics
} from './tools/index.js';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

// Create single log file for all projects
const logFile = path.join(os.tmpdir(), `claude-code-emacs-mcp.log`);
const logStream = fs.createWriteStream(logFile, { flags: 'a' });

function log(message: string) {
  const timestamp = new Date().toISOString();
  logStream.write(`[${timestamp}] ${message}\n`);
}

const projectRoot = process.cwd();
log(`Starting MCP server for project: ${projectRoot}...`);
log(`Log file: ${logFile}`);

// Create a new bridge instance for each MCP server
// This ensures isolation between different Claude Code sessions
const bridge = new EmacsBridge(log);
const server = new Server(
  {
    name: 'claude-code-emacs-mcp',
    version: '0.1.0',
  },
  {
    capabilities: {
      tools: {},
    },
  }
);

// Tool definitions
const TOOLS = [
  {
    name: 'openFile',
    description: 'Open a file in Emacs with optional text selection',
    inputSchema: {
      type: 'object',
      properties: {
        path: {
          type: 'string',
          description: 'File path relative to project root'
        },
        startText: {
          type: 'string',
          description: 'Text to start selection from (optional)'
        },
        endText: {
          type: 'string',
          description: 'Text to end selection at (optional)'
        }
      },
      required: ['path']
    }
  },
  {
    name: 'getOpenBuffers',
    description: 'Get list of open buffers in current project',
    inputSchema: {
      type: 'object',
      properties: {
        includeHidden: {
          type: 'boolean',
          description: 'Include hidden buffers (starting with space)',
          default: false
        }
      }
    }
  },
  {
    name: 'getCurrentSelection',
    description: 'Get current text selection in Emacs',
    inputSchema: {
      type: 'object',
      properties: {}
    }
  },
  {
    name: 'getDiagnostics',
    description: 'Get LSP diagnostics for project or specific buffer',
    inputSchema: {
      type: 'object',
      properties: {
        bufferPath: {
          type: 'string',
          description: 'Specific buffer path (optional, defaults to all project buffers)'
        }
      }
    }
  }
];

// Handle list tools request
server.setRequestHandler(ListToolsRequestSchema, async () => ({
  tools: TOOLS
}));

// Handle tool execution
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;

  try {
    switch (name) {
      case 'openFile':
        return await handleOpenFile(bridge, args || {});

      case 'getOpenBuffers':
        return await handleGetOpenBuffers(bridge, args || {});

      case 'getCurrentSelection':
        return await handleGetCurrentSelection(bridge, args || {});

      case 'getDiagnostics':
        return await handleGetDiagnostics(bridge, args || {});

      default:
        throw new Error(`Unknown tool: ${name}`);
    }
  } catch (error) {
    return {
      content: [
        {
          type: 'text',
          text: `Error: ${error instanceof Error ? error.message : String(error)}`
        }
      ]
    };
  }
});


// Notify Emacs about the port
async function notifyEmacsPort(port: number): Promise<void> {
  const projectRoot = process.cwd();
  const elisp = `(claude-code-emacs-mcp-register-port "${projectRoot}" ${port})`;

  // Try emacsclient first
  try {
    await execAsync(`emacsclient --eval '${elisp}'`);
    log(`Notified Emacs about port ${port} for project ${projectRoot}`);
  } catch (error) {
    log(`Failed to notify Emacs via emacsclient: ${error}`);
    // Continue even if notification fails - Emacs might not be running in server mode
  }

  // Also write port info to a file as fallback
  try {
    const portFile = path.join(os.tmpdir(), `claude-code-emacs-mcp-${projectRoot.replace(/[^a-zA-Z0-9]/g, '_')}.port`);
    await fs.promises.writeFile(portFile, JSON.stringify({ port, projectRoot }), 'utf8');
    log(`Wrote port info to ${portFile}`);
  } catch (error) {
    log(`Failed to write port file: ${error}`);
  }
}

// Start server
async function main() {
  // Use project root as session ID
  const sessionId = process.cwd();

  // Start Emacs bridge with port 0 for automatic assignment
  const port = await bridge.start(0, sessionId);

  // Notify Emacs about the assigned port
  await notifyEmacsPort(port);

  // For MCP, use stdio transport
  const transport = new StdioServerTransport();
  await server.connect(transport);

  log(`MCP server running for session ${sessionId}, Emacs bridge on port ${port}`);
}

// Cleanup on exit
process.on('SIGINT', async () => {
  log('Received SIGINT, shutting down...');
  await cleanup();
  process.exit(0);
});

process.on('SIGTERM', async () => {
  log('Received SIGTERM, shutting down...');
  await cleanup();
  process.exit(0);
});

// Handle uncaught exceptions and rejections
process.on('uncaughtException', (error) => {
  log(`Uncaught exception: ${error.message}`);
  log(`Stack: ${error.stack}`);
  log(`Project root: ${process.cwd()}`);
  cleanup().then(() => process.exit(1));
});

process.on('unhandledRejection', (reason, promise) => {
  log(`Unhandled rejection at: ${promise}, reason: ${reason}`);
  log(`Project root: ${process.cwd()}`);
  cleanup().then(() => process.exit(1));
});

async function cleanup() {
  const projectRoot = process.cwd();

  try {
    const elisp = `(claude-code-emacs-mcp-unregister-port "${projectRoot}")`;
    await execAsync(`emacsclient --eval '${elisp}'`);
    log(`Unregistered port for project ${projectRoot}`);
  } catch (error) {
    log(`Failed to unregister port: ${error}`);
  }

  // Clean up port file
  try {
    const portFile = path.join(os.tmpdir(), `claude-code-emacs-mcp-${projectRoot.replace(/[^a-zA-Z0-9]/g, '_')}.port`);
    await fs.promises.unlink(portFile);
    log(`Removed port file ${portFile}`);
  } catch (error) {
    log(`Failed to remove port file: ${error}`);
  }

  await bridge.stop();
}

main().catch((error) => {
  log(`Server error: ${error.message}`);
  log(`Stack: ${error.stack}`);
  log(`Project root: ${process.cwd()}`);
  log(`Process info: PID=${process.pid}, Node=${process.version}`);
  cleanup().then(() => process.exit(1));
});
