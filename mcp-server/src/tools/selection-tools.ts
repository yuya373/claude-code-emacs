import { EmacsBridge } from '../emacs-bridge.js';
import { GetCurrentSelectionArgs } from '../schemas/selection-schema.js';

interface SelectionInfo {
  text: string;
  startLine: number;
  endLine: number;
  startChar: number;
  endChar: number;
  fileName: string;
}

interface SelectionToolResult {
  content: Array<{ type: 'text'; text: string }>;
  selection?: string;
  file?: string;
  start?: { line: number; column: number };
  end?: { line: number; column: number };
  isError?: boolean;
}

export async function handleGetCurrentSelection(bridge: EmacsBridge, args: GetCurrentSelectionArgs): Promise<SelectionToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'Error: Emacs is not connected'
        }
      ],
      isError: true
    };
  }

  try {
    const result: SelectionInfo = await bridge.request('getCurrentSelection', args);

    if (!result.text) {
      return {
        content: [
          {
            type: 'text' as const,
            text: 'No text is currently selected'
          }
        ],
        // Return empty structured data when no selection
        selection: undefined,
        file: undefined,
        start: undefined,
        end: undefined
      };
    }

    const lineInfo = result.startLine === result.endLine
      ? `Line ${result.startLine}`
      : `Lines ${result.startLine}-${result.endLine}`;

    return {
      content: [
        {
          type: 'text' as const,
          text: `Selected text from ${result.fileName}:\n${lineInfo}, columns ${result.startChar}-${result.endChar}\n\n\`\`\`\n${result.text}\n\`\`\``
        }
      ],
      // Include structured data matching the schema
      selection: result.text,
      file: result.fileName,
      start: {
        line: result.startLine,
        column: result.startChar
      },
      end: {
        line: result.endLine,
        column: result.endChar
      }
    };
  } catch (error) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Error getting current selection: ${error instanceof Error ? error.message : 'Unknown error'}`
        }
      ],
      isError: true
    };
  }
}
