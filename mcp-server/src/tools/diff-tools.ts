import { EmacsBridge } from '../emacs-bridge';

interface DiffToolResponse {
  status: 'success' | 'error';
  message: string;
  file?: string;
}

// Helper function for openDiff
export async function handleOpenDiff(bridge: EmacsBridge, params: any) {
  // Validate parameters
  if (!params.fileA && !params.bufferA) {
    return {
      content: [{ type: 'text', text: 'Error: Either fileA/fileB or bufferA/bufferB must be provided' }]
    };
  }
  
  if (params.fileA && !params.fileB) {
    return {
      content: [{ type: 'text', text: 'Error: fileB is required when fileA is provided' }]
    };
  }
  
  if (params.bufferA && !params.bufferB) {
    return {
      content: [{ type: 'text', text: 'Error: bufferB is required when bufferA is provided' }]
    };
  }
  
  const mode = params.fileA ? 'files' : 'buffers';

  const response = await bridge.request('openDiff', {
    ...params,
    mode
  }) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  const items = mode === 'files'
    ? `files: ${params.fileA} and ${params.fileB}`
    : `buffers: ${params.bufferA} and ${params.bufferB}`;

  return {
    content: [{ type: 'text', text: `Opened ediff session for ${items}` }]
  };
}

// Helper function for openDiff3
export async function handleOpenDiff3(bridge: EmacsBridge, params: any) {
  const response = await bridge.request('openDiff3', params) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  if (params.ancestor) {
    return {
      content: [{ type: 'text', text: `Opened merge session with ancestor: ${params.ancestor}` }]
    };
  }

  return {
    content: [{ type: 'text', text: `Opened 3-way diff for: ${params.fileA}, ${params.fileB}, ${params.fileC}` }]
  };
}

// Helper function for openRevisionDiff
export async function handleOpenRevisionDiff(bridge: EmacsBridge, params: any) {
  const revision = params.revision || 'HEAD';

  const response = await bridge.request('openRevisionDiff', {
    file: params.file,
    revision
  }) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  return {
    content: [{ type: 'text', text: `Comparing ${params.file} with revision ${revision}` }]
  };
}

// Helper function for openCurrentChanges
export async function handleOpenCurrentChanges(bridge: EmacsBridge, params: any) {
  const response = await bridge.request('openCurrentChanges', params) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  const fileName = response.file || params.file || 'current file';
  return {
    content: [{ type: 'text', text: `Showing uncommitted changes for ${fileName}` }]
  };
}

// Helper function for applyPatch
export async function handleApplyPatch(bridge: EmacsBridge, params: any) {
  const response = await bridge.request('applyPatch', params) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  return {
    content: [{ type: 'text', text: `Applying patch ${params.patchFile} to ${params.targetFile}` }]
  };
}

export const diffTools = {
  openDiff: {
    name: 'openDiff',
    description: 'Open ediff to compare two files or buffers',
    inputSchema: {
      type: 'object',
      properties: {
        fileA: {
          type: 'string',
          description: 'Path to first file (relative to project root)'
        },
        fileB: {
          type: 'string',
          description: 'Path to second file (relative to project root)'
        },
        bufferA: {
          type: 'string',
          description: 'Name of first buffer (alternative to fileA)'
        },
        bufferB: {
          type: 'string',
          description: 'Name of second buffer (alternative to fileB)'
        }
      }
    },
    handler: handleOpenDiff
  },

  openDiff3: {
    name: 'openDiff3',
    description: 'Open ediff3 to compare three files',
    inputSchema: {
      type: 'object',
      properties: {
        fileA: {
          type: 'string',
          description: 'First file to compare'
        },
        fileB: {
          type: 'string',
          description: 'Second file to compare'
        },
        fileC: {
          type: 'string',
          description: 'Third file to compare'
        },
        ancestor: {
          type: 'string',
          description: 'Common ancestor for merge (optional)'
        }
      },
      required: ['fileA', 'fileB', 'fileC']
    },
    handler: handleOpenDiff3
  },

  openRevisionDiff: {
    name: 'openRevisionDiff',
    description: 'Compare file with its git revision',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File to compare'
        },
        revision: {
          type: 'string',
          description: 'Git revision (e.g., HEAD, HEAD~1, branch-name)',
          default: 'HEAD'
        }
      },
      required: ['file']
    },
    handler: handleOpenRevisionDiff
  },

  openCurrentChanges: {
    name: 'openCurrentChanges',
    description: 'Show current uncommitted changes in ediff',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File to show changes for (optional, defaults to current)'
        }
      }
    },
    handler: handleOpenCurrentChanges
  },

  applyPatch: {
    name: 'applyPatch',
    description: 'Apply a patch file using ediff',
    inputSchema: {
      type: 'object',
      properties: {
        patchFile: {
          type: 'string',
          description: 'Path to patch file'
        },
        targetFile: {
          type: 'string',
          description: 'File to apply patch to'
        }
      },
      required: ['patchFile', 'targetFile']
    },
    handler: handleApplyPatch
  }
};