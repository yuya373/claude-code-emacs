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

// Create log file
const logFile = path.join(os.tmpdir(), 'claude-code-emacs-mcp.log');
const logStream = fs.createWriteStream(logFile, { flags: 'a' });

function log(message: string) {
  const timestamp = new Date().toISOString();
  logStream.write(`[${timestamp}] ${message}\n`);
}

log('Starting MCP server...');

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

// Start server
async function main() {
  const port = parseInt(process.argv[2] || '8766');

  // Start Emacs bridge
  await bridge.start(port);

  // For MCP, use stdio transport
  const transport = new StdioServerTransport();
  await server.connect(transport);

  log(`MCP server running, Emacs bridge on port ${port}`);
}

main().catch((error) => {
  log(`Server error: ${error}`);
  process.exit(1);
});
