import { EmacsBridge } from '../emacs-bridge.js';

interface GetOpenBuffersArgs {
  includeHidden?: boolean;
}

interface Buffer {
  path: string;
  name: string;
  active: boolean;
  modified: boolean;
}

export async function handleGetOpenBuffers(bridge: EmacsBridge, args: GetOpenBuffersArgs) {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  const result = await bridge.request('getOpenBuffers', args);
  const buffers: Buffer[] = result.buffers;

  const bufferList = buffers.map(buf =>
    `${buf.active ? '* ' : '  '}${buf.name}${buf.modified ? ' [modified]' : ''}\n    ${buf.path}`
  ).join('\n');

  return {
    content: [
      {
        type: 'text',
        text: buffers.length > 0
          ? `Open buffers (${buffers.length}):\n\n${bufferList}`
          : 'No open buffers in current project'
      }
    ]
  };
}
