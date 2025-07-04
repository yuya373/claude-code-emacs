import { EmacsBridge } from '../emacs-bridge.js';
import {
  OpenDiffArgs,
  OpenRevisionDiffArgs,
  OpenCurrentChangesArgs,
  OpenDiffContentArgs,
  openDiffInputSchema,
  openRevisionDiffInputSchema,
  openCurrentChangesInputSchema,
  openDiffContentInputSchema,
  diffToolOutputSchema,
  DiffToolResult,
} from '../schemas/diff-schema.js';

type DiffToolResponse = DiffToolResult;

interface DiffToolHandlerResult {
  content: Array<{ type: 'text'; text: string }>;
  structuredContent: DiffToolResponse;
  isError?: boolean;
}

// Helper function for openDiff
export async function handleOpenDiff(bridge: EmacsBridge, params: OpenDiffArgs): Promise<DiffToolHandlerResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      structuredContent: {
        status: 'error' as const,
        message: 'Emacs is not connected'
      },
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
        structuredContent: response,
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Opened diff comparison for files: ${params.fileA} and ${params.fileB}` }],
      structuredContent: response
    };
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    return {
      content: [{ type: 'text' as const, text: `Error opening diff: ${errorMessage}` }],
      structuredContent: {
        status: 'error' as const,
        message: errorMessage
      },
      isError: true
    };
  }
}


// Helper function for openRevisionDiff
export async function handleOpenRevisionDiff(bridge: EmacsBridge, params: OpenRevisionDiffArgs): Promise<DiffToolHandlerResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      structuredContent: {
        status: 'error' as const,
        message: 'Emacs is not connected'
      },
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
        structuredContent: response,
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Comparing ${params.file} with revision ${revision}` }],
      structuredContent: response
    };
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    return {
      content: [{ type: 'text' as const, text: `Error opening revision diff: ${errorMessage}` }],
      structuredContent: {
        status: 'error' as const,
        message: errorMessage
      },
      isError: true
    };
  }
}

// Helper function for openCurrentChanges
export async function handleOpenCurrentChanges(bridge: EmacsBridge, params: OpenCurrentChangesArgs): Promise<DiffToolHandlerResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      structuredContent: {
        status: 'error' as const,
        message: 'Emacs is not connected'
      },
      isError: true
    };
  }

  try {
    const response = await bridge.request('openCurrentChanges', params) as DiffToolResponse;

    if (response.status === 'error') {
      return {
        content: [{ type: 'text' as const, text: `Error: ${response.message}` }],
        structuredContent: response,
        isError: true
      };
    }

    const fileName = response.file || params.file || 'current file';
    return {
      content: [{ type: 'text' as const, text: `Showing uncommitted changes for ${fileName}` }],
      structuredContent: response
    };
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    return {
      content: [{ type: 'text' as const, text: `Error showing current changes: ${errorMessage}` }],
      structuredContent: {
        status: 'error' as const,
        message: errorMessage
      },
      isError: true
    };
  }
}


// Helper function for openDiffContent
export async function handleOpenDiffContent(bridge: EmacsBridge, params: OpenDiffContentArgs): Promise<DiffToolHandlerResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{ type: 'text' as const, text: 'Error: Emacs is not connected' }],
      structuredContent: {
        status: 'error' as const,
        message: 'Emacs is not connected'
      },
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
        structuredContent: response,
        isError: true
      };
    }

    return {
      content: [{ type: 'text' as const, text: `Opened diff comparison for: ${params.titleA} and ${params.titleB}` }],
      structuredContent: response
    };
  } catch (error) {
    const errorMessage = error instanceof Error ? error.message : 'Unknown error';
    return {
      content: [{ type: 'text' as const, text: `Error opening diff content: ${errorMessage}` }],
      structuredContent: {
        status: 'error' as const,
        message: errorMessage
      },
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

  openDiffContent: {
    name: 'openDiffContent',
    description: 'Compare two text snippets or code blocks in temporary buffers. Use this for comparing content that is not saved in files, such as different versions of code snippets, API responses, or generated content.',
    inputSchema: openDiffContentInputSchema,
    outputSchema: diffToolOutputSchema,
    handler: handleOpenDiffContent
  }
};
