import { EmacsBridge } from '../emacs-bridge.js';

export interface GetDefinitionArgs {
  // File path and position to get definition for
  file?: string;
  line?: number;
  column?: number;
  // Alternative: symbol name to search for
  symbol?: string;
}

export interface DefinitionLocation {
  file: string;
  line: number;
  column: number;
  symbol: string;
  type?: string;
  preview?: string;
}

interface DefinitionResult {
  definitions: DefinitionLocation[];
  searchedSymbol?: string;
  method: 'lsp' | 'xref';
}

export async function handleGetDefinition(bridge: EmacsBridge, args: GetDefinitionArgs): Promise<any> {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  // Validate arguments
  if (!args.symbol && (!args.file || args.line === undefined)) {
    throw new Error('Either symbol or file/line must be provided');
  }

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
      output += `**Location**: Line ${def.line}, Column ${def.column}\n`;
      output += `**Symbol**: ${def.symbol}`;
      if (def.type) {
        output += ` (${def.type})`;
      }
      output += '\n';
      
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