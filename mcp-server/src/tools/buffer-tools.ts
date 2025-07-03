import { EmacsBridge } from '../emacs-bridge.js';
import { GetOpenBuffersArgs, Buffer } from '../schemas/buffer-schema.js';

interface BufferToolResult {
  content: Array<{ type: 'text'; text: string }>;
  buffers: Buffer[];
  isError?: boolean;
}

export async function handleGetOpenBuffers(bridge: EmacsBridge, args: GetOpenBuffersArgs): Promise<BufferToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{
        type: 'text' as const,
        text: 'Error: Emacs is not connected'
      }],
      buffers: [],
      isError: true
    };
  }

  try {
    const result = await bridge.request('getOpenBuffers', args);
    const buffers: Buffer[] = result.buffers;

    const bufferList = buffers.map(buf =>
      `${buf.active ? '* ' : '  '}${buf.name}${buf.modified ? ' [modified]' : ''}\n    ${buf.path}`
    ).join('\n');

    return {
      content: [
        {
          type: 'text' as const,
          text: buffers.length > 0
            ? `Open buffers (${buffers.length}):\n\n${bufferList}`
            : 'No open buffers in current project'
        }
      ],
      buffers // Include buffers in response for structured content
    };
  } catch (error) {
    return {
      content: [{
        type: 'text' as const,
        text: `Error getting open buffers: ${error instanceof Error ? error.message : 'Unknown error'}`
      }],
      buffers: [],
      isError: true
    };
  }
}
