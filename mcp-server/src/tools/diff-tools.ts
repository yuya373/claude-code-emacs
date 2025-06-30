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
    description: 'Compare two DIFFERENT files side-by-side in Emacs ediff. Use this to see differences between two distinct files. For comparing a file with its git history, use openRevisionDiff instead.',
    inputSchema: {
      type: 'object',
      properties: {
        fileA: {
          type: 'string',
          description: 'Path to the first file to compare (must be different from fileB)'
        },
        fileB: {
          type: 'string',
          description: 'Path to the second file to compare (must be different from fileA)'
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
    description: 'Compare a file with its previous version from git history. Use this when you want to see what changed in a file over time, not for comparing two different files.',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File to compare with its git history'
        },
        revision: {
          type: 'string',
          description: 'Git revision to compare against (e.g., HEAD, HEAD~1, branch-name, commit-hash)',
          default: 'HEAD'
        }
      },
      required: ['file']
    },
    handler: handleOpenRevisionDiff
  },

  openCurrentChanges: {
    name: 'openCurrentChanges',
    description: 'Show uncommitted git changes for a file in ediff. This compares the working copy with the last committed version (git diff).',
    inputSchema: {
      type: 'object',
      properties: {
        file: {
          type: 'string',
          description: 'File to show uncommitted changes for (optional, defaults to current file)'
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
    description: 'Compare two text snippets or code blocks in temporary buffers. Use this for comparing content that is not saved in files, such as different versions of code snippets, API responses, or generated content.',
    inputSchema: {
      type: 'object',
      properties: {
        contentA: {
          type: 'string',
          description: 'First text content to compare'
        },
        contentB: {
          type: 'string',
          description: 'Second text content to compare (should be different from contentA)'
        },
        titleA: {
          type: 'string',
          description: 'Descriptive title for the first content (e.g., "Original Code", "Version 1")'
        },
        titleB: {
          type: 'string',
          description: 'Descriptive title for the second content (e.g., "Modified Code", "Version 2")'
        }
      },
      required: ['contentA', 'contentB', 'titleA', 'titleB']
    },
    handler: handleOpenDiffContent
  }
};
