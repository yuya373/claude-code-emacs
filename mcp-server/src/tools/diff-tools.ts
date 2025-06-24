import { EmacsBridge } from '../emacs-bridge';

interface DiffToolResponse {
  status: 'success' | 'error';
  message: string;
  file?: string;
}

// Helper function for openDiff
export async function handleOpenDiff(bridge: EmacsBridge, params: any) {
  // Validate parameters
  if (!params.fileA || !params.fileB) {
    return {
      content: [{ type: 'text', text: 'Error: Both fileA and fileB are required' }]
    };
  }

  const response = await bridge.request('openDiff', {
    fileA: params.fileA,
    fileB: params.fileB
  }) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  return {
    content: [{ type: 'text', text: `Opened diff comparison for files: ${params.fileA} and ${params.fileB}` }]
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

// Helper function for openDiffContent
export async function handleOpenDiffContent(bridge: EmacsBridge, params: any) {
  // Validate parameters
  if (!params.contentA || !params.contentB || !params.titleA || !params.titleB) {
    return {
      content: [{ type: 'text', text: 'Error: contentA, contentB, titleA, and titleB are all required' }]
    };
  }

  const response = await bridge.request('openDiffContent', {
    contentA: params.contentA,
    contentB: params.contentB,
    titleA: params.titleA,
    titleB: params.titleB
  }) as DiffToolResponse;

  if (response.status === 'error') {
    return {
      content: [{ type: 'text', text: `Error: ${response.message}` }]
    };
  }

  return {
    content: [{ type: 'text', text: `Opened diff comparison for: ${params.titleA} and ${params.titleB}` }]
  };
}

export const diffTools = {
  openDiff: {
    name: 'openDiff',
    description: 'Open a visual diff tool to compare two files side-by-side',
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
        }
      },
      required: ['fileA', 'fileB']
    },
    handler: handleOpenDiff
  },

  openDiff3: {
    name: 'openDiff3',
    description: 'Open a three-way comparison tool for merge conflict resolution',
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
    description: 'Compare a file with a previous version from git history',
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
    description: 'Display uncommitted changes in a visual diff tool',
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
    description: 'Apply a patch file with interactive review of changes',
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
  },

  openDiffContent: {
    name: 'openDiffContent',
    description: 'Compare two text snippets side-by-side without creating files',
    inputSchema: {
      type: 'object',
      properties: {
        contentA: {
          type: 'string',
          description: 'Content for the first buffer'
        },
        contentB: {
          type: 'string',
          description: 'Content for the second buffer'
        },
        titleA: {
          type: 'string',
          description: 'Title for the first buffer'
        },
        titleB: {
          type: 'string',
          description: 'Title for the second buffer'
        }
      },
      required: ['contentA', 'contentB', 'titleA', 'titleB']
    },
    handler: handleOpenDiffContent
  }
};
