import { EmacsBridge } from '../emacs-bridge.js';

interface SelectionInfo {
  text: string;
  startLine: number;
  endLine: number;
  startChar: number;
  endChar: number;
  fileName: string;
}

export async function handleGetCurrentSelection(bridge: EmacsBridge, args: any) {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  const result: SelectionInfo = await bridge.request('getCurrentSelection', args);

  if (!result.text) {
    return {
      content: [
        {
          type: 'text',
          text: 'No text is currently selected'
        }
      ]
    };
  }

  const lineInfo = result.startLine === result.endLine 
    ? `Line ${result.startLine}` 
    : `Lines ${result.startLine}-${result.endLine}`;

  return {
    content: [
      {
        type: 'text',
        text: `Selected text from ${result.fileName}:\n${lineInfo}, columns ${result.startChar}-${result.endChar}\n\n\`\`\`\n${result.text}\n\`\`\``
      }
    ]
  };
}