import { EmacsBridge } from '../emacs-bridge.js';

export interface GetDefinitionArgs {
  // File path is required
  file: string;
  // Position within the file is required
  line: number;    // 1-based line number
  // Symbol name to search for
  symbol: string;
}

export interface Range {
  start: Position;
  end: Position;
}

export interface Position {
  line: number;
  // 0-based offset
  character: number;
}

export interface DefinitionLocation {
  file: string;
  preview?: string;
  range: Range;
}

interface DefinitionResult {
  definitions: DefinitionLocation[];
  method: 'lsp';
}

export async function handleGetDefinition(bridge: EmacsBridge, args: GetDefinitionArgs): Promise<any> {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  // Validate required parameters
  if (!args.file) {
    throw new Error('file parameter is required');
  }
  if (args.line === undefined || args.line === null) {
    throw new Error('line parameter is required');
  }
  if (!args.symbol) {
    throw new Error('symbol parameter is required');
  }

  try {
    const result = await bridge.request('getDefinition', args) as DefinitionResult;

    if (!result.definitions || result.definitions.length === 0) {
      return {
        content: [{
          type: 'text',
          text: `No definition found for ${args.symbol}`
        }]
      };
    }

    // Format the response
    const definitionCount = result.definitions.length;
    const plural = definitionCount > 1 ? 's' : '';
    let output = `Found ${definitionCount} definition${plural} using ${result.method}:\n\n`;

    result.definitions.forEach((def, index) => {
      output += `## Definition ${index + 1}\n`;
      output += `**File**: ${def.file}\n`;
      output += `**Location**: Line ${def.range.start.line + 1}, Column ${def.range.start.character + 1}\n`;

      if (def.preview) {
        output += '\n```\n' + def.preview + '\n```\n';
      }

      output += '\n';
    });

    return {
      content: [{
        type: 'text',
        text: output.trim()
      }]
    };
  } catch (error) {
    // Re-throw with more context
    if (error instanceof Error) {
      throw new Error(`Failed to get definition: ${error.message}`);
    }
    throw error;
  }
}
