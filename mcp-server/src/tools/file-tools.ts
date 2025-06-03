import { EmacsBridge } from '../emacs-bridge.js';

interface OpenFileArgs {
  path?: string;
  startText?: string;
  endText?: string;
}

export async function handleOpenFile(bridge: EmacsBridge, args: OpenFileArgs) {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  if (!args.path) {
    throw new Error('File path is required');
  }

  const result = await bridge.request('openFile', args);

  return {
    content: [
      {
        type: 'text',
        text: result.success
          ? `Opened file: ${result.path}${args.startText ? ' with text selection' : ''}`
          : `Failed to open file: ${args.path}`
      }
    ]
  };
}
