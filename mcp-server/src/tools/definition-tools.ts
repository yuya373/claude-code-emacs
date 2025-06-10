import { EmacsBridge } from '../emacs-bridge.js';

export interface GetDefinitionArgs {
  // File path is required
  file: string;
  // Position within the file is required
  line: number;
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
  symbol: string;
  preview?: string;
  range: Range;
}

interface DefinitionResult {
  definitions: DefinitionLocation[];
  searchedSymbol?: string;
  method: 'lsp';
}

export async function handleGetDefinition(bridge: EmacsBridge, args: GetDefinitionArgs): Promise<any> {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  // File is now always required (validated by TypeScript)
  // Additional validation for optional parameters is not needed

  try {
    const result = await bridge.request('getDefinition', args) as DefinitionResult;

    if (!result.definitions || result.definitions.length === 0) {
      return {
        content: [{
          type: 'text',
          text: `No definition found for ${result.searchedSymbol || 'the specified location'}`
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
      output += `**Symbol**: ${def.symbol}\n`;

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
