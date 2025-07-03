import { EmacsBridge } from '../emacs-bridge.js';
import {
  OpenDiffArgs,
  OpenDiff3Args,
  OpenRevisionDiffArgs,
  OpenCurrentChangesArgs,
  ApplyPatchArgs,
  OpenDiffContentArgs,
  openDiffInputSchema,
  openDiff3InputSchema,
  openRevisionDiffInputSchema,
  openCurrentChangesInputSchema,
  applyPatchInputSchema,
  openDiffContentInputSchema,
  diffToolOutputSchema
} from '../schemas/diff-schema.js';

interface DiffToolResponse {
  status: 'success' | 'error';
  message: string;
  file?: string;
}

interface DiffToolResult {
  content: Array<{ type: 'text'; text: string }>;
  isError?: boolean;
}

// Helper function for openDiff
export async function handleOpenDiff(bridge: EmacsBridge, params: OpenDiffArgs): Promise<DiffToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      isError: true
    };
  }

  try {
    const response = await bridge.request('openDiff', {
      fileA: params.fileA,
      fileB: params.fileB
    }) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Opened diff comparison for files: ${params.fileA} and ${params.fileB}` }]
    };
  } catch (error) {
    return {
      content: [{ type: 'text' as const, text: `Error opening diff: ${error instanceof Error ? error.message : 'Unknown error'}` }],
      isError: true
    };
  }
}

// Helper function for openDiff3
export async function handleOpenDiff3(bridge: EmacsBridge, params: OpenDiff3Args): Promise<DiffToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      isError: true
    };
  }

  try {
    const response = await bridge.request('openDiff3', params) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        isError: true
      };
    }

    if (params.ancestor) {
      return {
        content: [{ type: 'text' as const, text: `Opened merge session with ancestor: ${params.ancestor}` }]
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Opened 3-way diff for: ${params.fileA}, ${params.fileB}, ${params.fileC}` }]
    };
  } catch (error) {
    return {
      content: [{ type: 'text' as const, text: `Error opening diff3: ${error instanceof Error ? error.message : 'Unknown error'}` }],
      isError: true
    };
  }
}

// Helper function for openRevisionDiff
export async function handleOpenRevisionDiff(bridge: EmacsBridge, params: OpenRevisionDiffArgs): Promise<DiffToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      isError: true
    };
  }

  try {
    const revision = params.revision || 'HEAD';

    const response = await bridge.request('openRevisionDiff', {
      file: params.file,
      revision
    }) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Comparing ${params.file} with revision ${revision}` }]
    };
  } catch (error) {
    return {
      content: [{ type: 'text' as const, text: `Error opening revision diff: ${error instanceof Error ? error.message : 'Unknown error'}` }],
      isError: true
    };
  }
}

// Helper function for openCurrentChanges
export async function handleOpenCurrentChanges(bridge: EmacsBridge, params: OpenCurrentChangesArgs): Promise<DiffToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      isError: true
    };
  }

  try {
    const response = await bridge.request('openCurrentChanges', params) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        isError: true
      };
    }

    const fileName = response.file || params.file || 'current file';
    return {
      content: [{ type: 'text' as const, text: `Showing uncommitted changes for ${fileName}` }]
    };
  } catch (error) {
    return {
      content: [{ type: 'text' as const, text: `Error showing current changes: ${error instanceof Error ? error.message : 'Unknown error'}` }],
      isError: true
    };
  }
}

// Helper function for applyPatch
export async function handleApplyPatch(bridge: EmacsBridge, params: ApplyPatchArgs): Promise<DiffToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      isError: true
    };
  }

  try {
    const response = await bridge.request('applyPatch', params) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Applying patch ${params.patchFile} to ${params.targetFile}` }]
    };
  } catch (error) {
    return {
      content: [{ type: 'text' as const, text: `Error applying patch: ${error instanceof Error ? error.message : 'Unknown error'}` }],
      isError: true
    };
  }
}

// Helper function for openDiffContent
export async function handleOpenDiffContent(bridge: EmacsBridge, params: OpenDiffContentArgs): Promise<DiffToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      isError: true
    };
  }

  try {
    const response = await bridge.request('openDiffContent', {
      contentA: params.contentA,
      contentB: params.contentB,
      titleA: params.titleA,
      titleB: params.titleB
    }) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Opened diff comparison for: ${params.titleA} and ${params.titleB}` }]
    };
  } catch (error) {
    return {
      content: [{ type: 'text' as const, text: `Error opening diff content: ${error instanceof Error ? error.message : 'Unknown error'}` }],
      isError: true
    };
  }
}

export const diffTools = {
  openDiff: {
    name: 'openDiff',
    description: 'Compare two DIFFERENT files side-by-side in Emacs ediff. Use this to see differences between two distinct files. For comparing a file with its git history, use openRevisionDiff instead.',
    inputSchema: openDiffInputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleOpenDiff
  },

  openDiff3: {
    name: 'openDiff3',
    description: 'Open a three-way comparison tool for merge conflict resolution',
    inputSchema: openDiff3InputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleOpenDiff3
  },

  openRevisionDiff: {
    name: 'openRevisionDiff',
    description: 'Compare a file with its previous version from git history. Use this when you want to see what changed in a file over time, not for comparing two different files.',
    inputSchema: openRevisionDiffInputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleOpenRevisionDiff
  },

  openCurrentChanges: {
    name: 'openCurrentChanges',
    description: 'Show uncommitted git changes for a file in ediff. This compares the working copy with the last committed version (git diff).',
    inputSchema: openCurrentChangesInputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleOpenCurrentChanges
  },

  applyPatch: {
    name: 'applyPatch',
    description: 'Apply a patch file with interactive review of changes',
    inputSchema: applyPatchInputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleApplyPatch
  },

  openDiffContent: {
    name: 'openDiffContent',
    description: 'Compare two text snippets or code blocks in temporary buffers. Use this for comparing content that is not saved in files, such as different versions of code snippets, API responses, or generated content.',
    inputSchema: openDiffContentInputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleOpenDiffContent
  }
};
