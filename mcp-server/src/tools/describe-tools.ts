import { EmacsBridge } from '../emacs-bridge.js';

export interface DescribeSymbolArgs {
  // File path is required
  file: string;
  // Position within the file is required
  line: number;    // 1-based line number
  // Symbol name to describe
  symbol: string;
}

export interface SymbolDescription {
  documentation?: string;
}

interface DescribeSymbolResult {
  description: SymbolDescription;
  method: 'lsp';
}

export async function handleDescribeSymbol(bridge: EmacsBridge, args: DescribeSymbolArgs): Promise<any> {
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
    const result = await bridge.request('describeSymbol', args) as DescribeSymbolResult;

    if (!result.description || !result.description.documentation) {
      return {
        content: [{
          type: 'text',
          text: `No documentation found for ${args.symbol}`
        }]
      };
    }

    // Simply return the documentation
    const output = result.description.documentation;

    return {
      content: [{
        type: 'text',
        text: output.trim()
      }]
    };
  } catch (error) {
    // Re-throw with more context
    if (error instanceof Error) {
      throw new Error(`Failed to describe symbol: ${error.message}`);
    }
    throw error;
  }
}
