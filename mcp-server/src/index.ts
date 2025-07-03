#!/usr/bin/env node

import { McpServer, ResourceTemplate } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { EmacsBridge } from './emacs-bridge.js';
import { sendNotificationInputSchema, sendNotificationOutputSchema } from './schemas/notification-schema.js';
import { getDiagnosticsInputSchema, getDiagnosticsOutputSchema } from './schemas/diagnostic-schema.js';
import { getDefinitionInputSchema, getDefinitionOutputSchema } from './schemas/definition-schema.js';
import { findReferencesInputSchema, findReferencesOutputSchema } from './schemas/reference-schema.js';
import { describeSymbolInputSchema, describeSymbolOutputSchema } from './schemas/describe-schema.js';
import { getOpenBuffersInputSchema, getOpenBuffersOutputSchema } from './schemas/buffer-schema.js';
import { getCurrentSelectionInputSchema, getCurrentSelectionOutputSchema } from './schemas/selection-schema.js';
import {
  handleGetOpenBuffers,
  handleGetCurrentSelection,
  handleGetDiagnostics,
  diffTools,
  handleGetDefinition,
  handleFindReferences,
  handleDescribeSymbol,
  handleSendNotification
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
const server = new McpServer(
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
  server.server.notification({
    method: method,
    params: params
  });
});


// Register tools with McpServer
function registerTools() {
  // getOpenBuffers tool
  server.registerTool('getOpenBuffers', {
    description: 'Get list of open buffers in current project',
    inputSchema: getOpenBuffersInputSchema.shape,
    outputSchema: getOpenBuffersOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleGetOpenBuffers(bridge, args);
    return {
      content: result.content,
      structuredContent: { buffers: result.buffers },
      isError: result.isError
    };
  });

  // getCurrentSelection tool
  server.registerTool('getCurrentSelection', {
    description: 'Get current text selection in Emacs',
    inputSchema: getCurrentSelectionInputSchema.shape,
    outputSchema: getCurrentSelectionOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleGetCurrentSelection(bridge, args);
    return {
      content: result.content,
      structuredContent: {
        selection: result.selection,
        file: result.file,
        start: result.start,
        end: result.end
      },
      isError: result.isError
    };
  });

  // getDiagnostics tool
  server.registerTool('getDiagnostics', {
    description: 'Get project-wide LSP diagnostics using specified buffer for LSP context',
    inputSchema: getDiagnosticsInputSchema.shape,
    outputSchema: getDiagnosticsOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleGetDiagnostics(bridge, args);
    return {
      content: result.content,
      structuredContent: { diagnostics: result.diagnostics },
      isError: result.isError
    };
  });

  // getDefinition tool
  server.registerTool('getDefinition', {
    description: 'Find definition of symbol using LSP',
    inputSchema: getDefinitionInputSchema.shape,
    outputSchema: getDefinitionOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleGetDefinition(bridge, args);
    return {
      content: result.content,
      structuredContent: { definitions: result.definitions },
      isError: result.isError
    };
  });

  // findReferences tool
  server.registerTool('findReferences', {
    description: 'Find all references to a symbol using LSP',
    inputSchema: findReferencesInputSchema.shape,
    outputSchema: findReferencesOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleFindReferences(bridge, args);
    return {
      content: result.content,
      structuredContent: { references: result.references },
      isError: result.isError
    };
  });

  // describeSymbol tool
  server.registerTool('describeSymbol', {
    description: 'Get full documentation and information about a symbol using LSP hover',
    inputSchema: describeSymbolInputSchema.shape,
    outputSchema: describeSymbolOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleDescribeSymbol(bridge, args);
    return {
      content: result.content,
      structuredContent: {
        documentation: result.documentation
      },
      isError: result.isError
    };
  });

  // sendNotification tool
  server.registerTool('sendNotification', {
    description: 'Send a desktop notification to alert the user when tasks complete or need attention',
    inputSchema: sendNotificationInputSchema.shape,
    outputSchema: sendNotificationOutputSchema.shape
  }, async (args, _extra) => {
    const result = await handleSendNotification(bridge, args);
    return {
      content: result.content,
      structuredContent: {
        status: result.status,
        message: result.message,
      },
      isError: result.isError
    };
  });

  // Register diff tools individually for better type inference
  // openDiff tool
  server.registerTool('openDiff', {
    description: diffTools.openDiff.description,
    inputSchema: diffTools.openDiff.inputSchema.shape,
    outputSchema: diffTools.openDiff.outputSchema.shape
  }, async (args, _extra) => {
    const result = await diffTools.openDiff.handler(bridge, args);
    return {
      content: result.content,
      isError: result.isError
    };
  });

  // openDiff3 tool
  server.registerTool('openDiff3', {
    description: diffTools.openDiff3.description,
    inputSchema: diffTools.openDiff3.inputSchema.shape,
    outputSchema: diffTools.openDiff3.outputSchema.shape
  }, async (args, _extra) => {
    const result = await diffTools.openDiff3.handler(bridge, args);
    return {
      content: result.content,
      isError: result.isError
    };
  });

  // openRevisionDiff tool
  server.registerTool('openRevisionDiff', {
    description: diffTools.openRevisionDiff.description,
    inputSchema: diffTools.openRevisionDiff.inputSchema.shape,
    outputSchema: diffTools.openRevisionDiff.outputSchema.shape
  }, async (args, _extra) => {
    const result = await diffTools.openRevisionDiff.handler(bridge, args);
    return {
      content: result.content,
      isError: result.isError
    };
  });

  // openCurrentChanges tool
  server.registerTool('openCurrentChanges', {
    description: diffTools.openCurrentChanges.description,
    inputSchema: diffTools.openCurrentChanges.inputSchema.shape,
    outputSchema: diffTools.openCurrentChanges.outputSchema.shape
  }, async (args, _extra) => {
    const result = await diffTools.openCurrentChanges.handler(bridge, args);
    return {
      content: result.content,
      isError: result.isError
    };
  });

  // applyPatch tool
  server.registerTool('applyPatch', {
    description: diffTools.applyPatch.description,
    inputSchema: diffTools.applyPatch.inputSchema.shape,
    outputSchema: diffTools.applyPatch.outputSchema.shape
  }, async (args, _extra) => {
    const result = await diffTools.applyPatch.handler(bridge, args);
    return {
      content: result.content,
      isError: result.isError
    };
  });

  // openDiffContent tool
  server.registerTool('openDiffContent', {
    description: diffTools.openDiffContent.description,
    inputSchema: diffTools.openDiffContent.inputSchema.shape,
    outputSchema: diffTools.openDiffContent.outputSchema.shape
  }, async (args, _extra) => {
    const result = await diffTools.openDiffContent.handler(bridge, args);
    return {
      content: result.content,
      isError: result.isError
    };
  });
}

// Register resources with McpServer
function registerResources() {
  // Buffer resources (dynamic)
  const bufferTemplate = new ResourceTemplate(
    'file://{path}',
    {
      list: async () => {
        try {
          const resources = await bufferResourceHandler.list(bridge);
          log(`Listed ${resources.length} buffer resources`);
          return { resources };
        } catch (error) {
          log(`Error listing buffer resources: ${error}`);
          return { resources: [] };
        }
      }
    }
  );

  server.resource(
    'emacs-buffers',
    bufferTemplate,
    {
      description: 'Open buffers in Emacs',
      mimeType: 'text/plain'
    },
    async (uri, _variables, _extra) => {
      log(`Reading buffer resource: ${uri}`);
      const result = await bufferResourceHandler.read(bridge, uri.toString());
      return {
        contents: [{
          uri: uri.toString(),
          mimeType: result.mimeType || 'text/plain',
          text: result.text as string
        }]
      };
    }
  );

  // Project info resource (static)
  server.resource(
    'project-info',
    'emacs://project/info',
    {
      description: 'Current project information',
      mimeType: 'application/json'
    },
    async (uri, _extra) => {
      log(`Reading project resource: ${uri}`);
      const result = await projectResourceHandler.read(bridge, uri.toString());
      return {
        contents: [{
          uri: uri.toString(),
          mimeType: result.mimeType || 'application/json',
          text: result.text as string
        }]
      };
    }
  );

  // Diagnostics resources (dynamic)
  const diagnosticsTemplate = new ResourceTemplate(
    'emacs://diagnostics/{file}',
    {
      list: async () => {
        try {
          const resources = await diagnosticsResourceHandler.list(bridge);
          log(`Listed ${resources.length} diagnostics resources`);
          return { resources };
        } catch (error) {
          log(`Error listing diagnostics resources: ${error}`);
          return { resources: [] };
        }
      }
    }
  );

  server.resource(
    'lsp-diagnostics',
    diagnosticsTemplate,
    {
      description: 'LSP diagnostics for project files',
      mimeType: 'application/json'
    },
    async (uri, _variables, _extra) => {
      log(`Reading diagnostics resource: ${uri}`);
      const result = await diagnosticsResourceHandler.read(bridge, uri.toString());
      return {
        contents: [{
          uri: uri.toString(),
          mimeType: result.mimeType || 'application/json',
          text: result.text as string
        }]
      };
    }
  );
}


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

  // Register tools and resources
  registerTools();
  registerResources();

  const ping = async () => {
    try {
      await server.server.ping()
      log(`Ping successful for session ${sessionId}`);
      setTimeout(ping, 30000)

    } catch (error) {
      log(`Ping failed for session ${sessionId}, Emacs bridge on port ${port}. Exitting...`);
      await cleanup();
      process.exit(1)
    }
  }
  server.server.oninitialized = () => {
    log(`MCP server initialized for session ${sessionId}, Emacs bridge on port ${port}`);
    log(`Starting ping monitoring for session ${sessionId}`);
    ping()
    const cap = server.server.getClientCapabilities()
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
