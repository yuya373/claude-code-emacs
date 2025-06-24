import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { CallToolRequestSchema, ClientNotificationSchema, ListResourcesRequestSchema, ListToolsRequestSchema, ReadResourceRequestSchema } from '@modelcontextprotocol/sdk/types.js';
import { EmacsBridge } from './emacs-bridge.js';
import {
  handleGetOpenBuffers,
  handleGetCurrentSelection,
  handleGetDiagnostics,
  GetDiagnosticsArgs,
  diffTools,
  handleOpenDiff,
  handleOpenDiff3,
  handleOpenRevisionDiff,
  handleOpenCurrentChanges,
  handleApplyPatch,
  handleOpenDiffContent,
  handleGetDefinition,
  GetDefinitionArgs,
  handleFindReferences,
  FindReferencesArgs,
  handleDescribeSymbol,
  DescribeSymbolArgs,
  handleSendNotification,
  NotificationArgs
} from './tools/index.js';
import {
  bufferResourceHandler,
  projectResourceHandler,
  diagnosticsResourceHandler
} from './resources/index.js';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { exec } from 'child_process';
import { promisify } from 'util';

const execAsync = promisify(exec);

// Normalize project root by removing trailing slash
function normalizeProjectRoot(root: string): string {
  return root.replace(/\/$/, '');
}

// Create log file in project root
const projectRoot = normalizeProjectRoot(process.cwd());
const logFile = path.join(projectRoot, '.claude-code-emacs-mcp.log');
const logStream = fs.createWriteStream(logFile, { flags: 'a' });

function log(message: string) {
  const timestamp = new Date().toISOString();
  logStream.write(`[${timestamp}] ${message}\n`);
}

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
      resources: {},
    },
  }
);

// Set up notification handler to forward Emacs events to Claude Code
bridge.setNotificationHandler((method: string, params: any) => {
  log(`Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`);
  server.notification({
    method: method,
    params: params
  });
});


// Tool definitions
const TOOLS = [
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
    description: 'Get project-wide LSP diagnostics using specified buffer for LSP context',
    inputSchema: {
      type: 'object',
      properties: {
        buffer: {
          type: 'string',
          description: 'Buffer name to execute lsp-diagnostics in (required for LSP workspace context)'
        }
      },
      required: ['buffer']
    }
  },
  {
    name: 'getDefinition',
    description: 'Find definition of symbol using LSP',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File path to search from (required)'
        },
        line: {
          type: 'number',
          description: 'Line number (1-based, required)'
        },
        symbol: {
          type: 'string',
          description: 'Symbol name to search for (required)'
        }
      },
      required: ['file', 'line', 'symbol']
    }
  },
  {
    name: 'findReferences',
    description: 'Find all references to a symbol using LSP',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File path to search from (required)'
        },
        line: {
          type: 'number',
          description: 'Line number (1-based, required)'
        },
        symbol: {
          type: 'string',
          description: 'Symbol name to search for (required)'
        },
        includeDeclaration: {
          type: 'boolean',
          description: 'Include the declaration in results (default: true)'
        }
      },
      required: ['file', 'line', 'symbol']
    }
  },
  {
    name: 'describeSymbol',
    description: 'Get full documentation and information about a symbol using LSP hover',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File path relative to project root'
        },
        line: {
          type: 'number',
          description: 'Line number (1-based)'
        },
        symbol: {
          type: 'string',
          description: 'Symbol name to describe'
        }
      },
      required: ['file', 'line', 'symbol']
    }
  },
  {
    name: 'sendNotification',
    description: 'Send a desktop notification to alert the user when tasks complete or need attention',
    inputSchema: {
      type: 'object',
      properties: {
        title: {
          type: 'string',
          description: 'Title of the notification'
        },
        message: {
          type: 'string',
          description: 'Message content of the notification'
        }
      },
      required: ['title', 'message']
    }
  },
  // Add diff tools
  ...Object.values(diffTools).map(tool => ({
    name: tool.name,
    description: tool.description,
    inputSchema: tool.inputSchema
  }))
];

// Handle list tools request
server.setRequestHandler(ListToolsRequestSchema, async (request) => {
  log(`MCP Request: list tools`);
  return {
    tools: TOOLS
  };
});

// Handle list resources request
server.setRequestHandler(ListResourcesRequestSchema, async (request) => {
  log(`MCP Request: list resources`);
  try {
    const resources = [
      ...(await bufferResourceHandler.list(bridge)),
      ...(await projectResourceHandler.list(bridge)),
      ...(await diagnosticsResourceHandler.list(bridge))
    ];

    log(`MCP Response: returning ${resources.length} resources`);
    return { resources };
  } catch (error) {
    log(`Error listing resources: ${error}`);
    return { resources: [] };
  }
});

// Handle read resource request
server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
  const { uri } = request.params;
  log(`MCP Request: read resource - ${uri}`);

  try {
    let result;
    // Route to appropriate handler based on URI scheme
    if (uri.startsWith('file://')) {
      result = await bufferResourceHandler.read(bridge, uri);
    } else if (uri.startsWith('emacs://project/')) {
      result = await projectResourceHandler.read(bridge, uri);
    } else if (uri.startsWith('emacs://diagnostics/')) {
      result = await diagnosticsResourceHandler.read(bridge, uri);
    } else {
      throw new Error(`Unsupported resource URI scheme: ${uri}`);
    }

    log(`MCP Response: read resource successful - ${uri}`);
    return result;
  } catch (error) {
    log(`Error reading resource ${uri}: ${error}`);
    throw error;
  }
});

// Handle tool execution
server.setRequestHandler(CallToolRequestSchema, async (request) => {
  const { name, arguments: args } = request.params;
  log(`MCP Request: call tool - ${name} with args: ${JSON.stringify(args)}`);

  try {
    let result;
    switch (name) {

      case 'getOpenBuffers':
        result = await handleGetOpenBuffers(bridge, args || {});
        break;

      case 'getCurrentSelection':
        result = await handleGetCurrentSelection(bridge, args || {});
        break;

      case 'getDiagnostics':
        result = await handleGetDiagnostics(bridge, (args || {}) as unknown as GetDiagnosticsArgs);
        break;

      case 'openDiff':
        result = await handleOpenDiff(bridge, args || {});
        break;

      case 'openDiff3':
        result = await handleOpenDiff3(bridge, args || {});
        break;

      case 'openRevisionDiff':
        result = await handleOpenRevisionDiff(bridge, args || {});
        break;

      case 'openCurrentChanges':
        result = await handleOpenCurrentChanges(bridge, args || {});
        break;

      case 'applyPatch':
        result = await handleApplyPatch(bridge, args || {});
        break;

      case 'openDiffContent':
        result = await handleOpenDiffContent(bridge, args || {});
        break;

      case 'getDefinition':
        result = await handleGetDefinition(bridge, (args || {}) as unknown as GetDefinitionArgs);
        break;

      case 'findReferences':
        result = await handleFindReferences(bridge, (args || {}) as unknown as FindReferencesArgs);
        break;

      case 'describeSymbol':
        result = await handleDescribeSymbol(bridge, (args || {}) as unknown as DescribeSymbolArgs);
        break;

      case 'sendNotification':
        result = await handleSendNotification(bridge, (args || {}) as unknown as NotificationArgs);
        break;

      default:
        throw new Error(`Unknown tool: ${name}`);
    }

    log(`MCP Response: tool ${name} executed successfully`);
    return result;
  } catch (error) {
    log(`MCP Error: tool ${name} failed - ${error instanceof Error ? error.message : String(error)}`);
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
  const projectRoot = normalizeProjectRoot(process.cwd());
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
  const sessionId = normalizeProjectRoot(process.cwd());

  // Start Emacs bridge with port 0 for automatic assignment
  const port = await bridge.start(0, sessionId);

  // Notify Emacs about the assigned port
  await notifyEmacsPort(port);

  const ping = async () => {
    try {
      await server.ping()
      log(`Ping successful for session ${sessionId}`);
      setTimeout(ping, 30000)

    } catch (error) {
      log(`Ping failed for session ${sessionId}, Emacs bridge on port ${port}. Exitting...`);
      await cleanup();
      process.exit(1)
    }
  }
  server.oninitialized = () => {
    log(`MCP server initialized for session ${sessionId}, Emacs bridge on port ${port}`);
    log(`Starting ping monitoring for session ${sessionId}`);
    ping()
    const cap = server.getClientCapabilities()
    log(`Client capabilities: ${JSON.stringify(cap)}`)
  }

  // For MCP, use stdio transport
  const transport = new StdioServerTransport();
  await server.connect(transport)
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
  log(`Project root: ${normalizeProjectRoot(process.cwd())}`);
  cleanup().then(() => process.exit(1));
});

process.on('unhandledRejection', (reason, promise) => {
  log(`Unhandled rejection at: ${promise}, reason: ${reason}`);
  log(`Project root: ${normalizeProjectRoot(process.cwd())}`);
  cleanup().then(() => process.exit(1));
});

async function cleanup() {
  const projectRoot = normalizeProjectRoot(process.cwd());

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
  log(`Project root: ${normalizeProjectRoot(process.cwd())}`);
  log(`Process info: PID=${process.pid}, Node=${process.version}`);
  cleanup().then(() => process.exit(1));
});
