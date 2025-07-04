import { Resource, ResourceContents, TextResourceContents } from '@modelcontextprotocol/sdk/types.js';
import { EmacsBridge } from '../emacs-bridge.js';
import { ResourceHandler } from './index.js';

export const diagnosticsResourceHandler: ResourceHandler = {
  async list(bridge: EmacsBridge): Promise<Resource[]> {
    const resources: Resource[] = [
      {
        uri: 'emacs://diagnostics/all',
        name: 'All Diagnostics',
        description: 'LSP diagnostics for all open buffers',
        mimeType: 'application/json'
      }
    ];

    try {
      // Get list of buffers with diagnostics
      const result = await bridge.sendRequest('get-diagnostics-resource', {});

      if (result.success && result.diagnostics) {
        // Add individual buffer diagnostic resources
        for (const [bufferPath, diags] of Object.entries(result.diagnostics)) {
          if (Array.isArray(diags) && diags.length > 0) {
            resources.push({
              uri: `emacs://diagnostics/buffer?path=${encodeURIComponent(bufferPath)}`,
              name: `Diagnostics: ${bufferPath}`,
              description: `${diags.length} diagnostic(s)`,
              mimeType: 'application/json'
            });
          }
        }
      }
    } catch (error) {
      console.error('Error listing diagnostic resources:', error);
    }

    return resources;
  },

  async read(bridge: EmacsBridge, uri: string): Promise<ResourceContents> {
    if (uri === 'emacs://diagnostics/all') {
      return await getAllDiagnostics(bridge);
    }

    // Parse buffer-specific diagnostic URI
    const url = new URL(uri);
    if (url.pathname === '//diagnostics/buffer') {
      const bufferPath = url.searchParams.get('path');
      if (bufferPath) {
        return await getBufferDiagnostics(bridge, bufferPath);
      }
    }

    throw new Error(`Unknown diagnostics resource: ${uri}`);
  }
};

async function getAllDiagnostics(bridge: EmacsBridge): Promise<TextResourceContents> {
  try {
    const result = await bridge.sendRequest('get-diagnostics-resource', {});

    if (!result.success) {
      throw new Error(result.error || 'Failed to get diagnostics');
    }

    const diagnostics = result.diagnostics || {};
    const summary = {
      totalBuffers: Object.keys(diagnostics).length,
      totalDiagnostics: Object.values(diagnostics).reduce((sum: number, diags: any) =>
        sum + (Array.isArray(diags) ? diags.length : 0), 0),
      diagnostics: diagnostics
    };

    return {
      uri: 'emacs://diagnostics/all',
      mimeType: 'application/json',
      text: JSON.stringify(summary, null, 2)
    };
  } catch (error) {
    return {
      uri: 'emacs://diagnostics/all',
      mimeType: 'application/json',
      text: JSON.stringify({ error: error instanceof Error ? error.message : String(error) }, null, 2)
    };
  }
}

async function getBufferDiagnostics(bridge: EmacsBridge, bufferPath: string): Promise<TextResourceContents> {
  try {
    const result = await bridge.sendRequest('get-diagnostics-resource', { bufferPath });

    if (!result.success) {
      throw new Error(result.error || 'Failed to get buffer diagnostics');
    }

    const diagnostics = result.diagnostics || [];

    return {
      uri: `emacs://diagnostics/buffer?path=${encodeURIComponent(bufferPath)}`,
      mimeType: 'application/json',
      text: JSON.stringify({
        buffer: bufferPath,
        count: diagnostics.length,
        diagnostics: diagnostics
      }, null, 2)
    };
  } catch (error) {
    return {
      uri: `emacs://diagnostics/buffer?path=${encodeURIComponent(bufferPath)}`,
      mimeType: 'application/json',
      text: JSON.stringify({
        buffer: bufferPath,
        error: error instanceof Error ? error.message : String(error)
      }, null, 2)
    };
  }
}
