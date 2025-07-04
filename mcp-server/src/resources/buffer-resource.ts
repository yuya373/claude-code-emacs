import { Resource, ResourceContents, TextResourceContents } from '@modelcontextprotocol/sdk/types.js';
import { EmacsBridge } from '../emacs-bridge.js';
import { ResourceHandler } from './index.js';
import * as path from 'path';

export const bufferResourceHandler: ResourceHandler = {
  async list(bridge: EmacsBridge): Promise<Resource[]> {
    try {
      const result = await bridge.sendRequest('get-open-buffers', { includeHidden: false });
      console.log('Buffer list result:', JSON.stringify(result, null, 2));

      // The response from Emacs doesn't include 'success' field, just check for buffers array
      if (!result || !Array.isArray(result.buffers)) {
        console.log('Returning empty array due to failed check');
        return [];
      }

      return result.buffers.map((buffer: any) => ({
        uri: `emacs://buffer${buffer.path}`,  // No extra slash needed
        name: path.basename(buffer.path),
        description: `Buffer: ${buffer.path}`,
        mimeType: getMimeType(buffer.path)
      }));
    } catch (error) {
      console.error('Error listing buffer resources:', error);
      return [];
    }
  },

  async read(bridge: EmacsBridge, uri: string): Promise<ResourceContents> {
    try {
      // Extract file path from URI
      const filePath = uri.replace('emacs://buffer', '');  // Remove without trailing slash

      const result = await bridge.sendRequest('get-buffer-content', { path: filePath });

      // Check if content exists in the response
      if (!result || typeof result.content !== 'string') {
        throw new Error(result?.error || 'Failed to read buffer content');
      }

      return {
        uri,
        mimeType: getMimeType(filePath),
        text: result.content
      } as TextResourceContents;
    } catch (error) {
      throw new Error(`Failed to read buffer resource: ${error}`);
    }
  }
};

function getMimeType(filePath: string): string {
  const ext = path.extname(filePath).toLowerCase();
  const mimeTypes: Record<string, string> = {
    '.js': 'text/javascript',
    '.ts': 'text/typescript',
    '.jsx': 'text/jsx',
    '.tsx': 'text/tsx',
    '.json': 'application/json',
    '.md': 'text/markdown',
    '.html': 'text/html',
    '.css': 'text/css',
    '.py': 'text/x-python',
    '.rb': 'text/x-ruby',
    '.java': 'text/x-java',
    '.c': 'text/x-c',
    '.cpp': 'text/x-c++',
    '.el': 'text/x-emacs-lisp',
    '.yml': 'text/yaml',
    '.yaml': 'text/yaml',
    '.xml': 'text/xml',
    '.sh': 'text/x-shellscript',
    '.txt': 'text/plain'
  };

  return mimeTypes[ext] || 'text/plain';
}
