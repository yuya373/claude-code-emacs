import { EmacsBridge } from '../emacs-bridge.js';
import { DescribeSymbolArgs } from '../schemas/describe-schema.js';

export { DescribeSymbolArgs };

export interface SymbolDescription {
  documentation?: string;
}

interface DescribeSymbolResult {
  description: SymbolDescription;
  method: 'lsp';
}
interface DescribeSymbolToolResult {
  content: Array<{ type: 'text'; text: string }>;
  documentation?: string;
  isError?: boolean;
}

export async function handleDescribeSymbol(bridge: EmacsBridge, args: DescribeSymbolArgs): Promise<DescribeSymbolToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{
        type: 'text' as const,
        text: 'Error: Emacs is not connected'
      }],
      isError: true
    };
  }

  try {
    const result = await bridge.request('describeSymbol', args) as DescribeSymbolResult;

    if (!result.description || !result.description.documentation) {
      return {
        content: [{
          type: 'text' as const,
          text: `No documentation found for ${args.symbol}`
        }]
      };
    }

    // Return both content and structured data
    const output = result.description.documentation;

    return {
      content: [{
        type: 'text' as const,
        text: output.trim()
      }],
      // Include structured data for the output schema
      documentation: result.description.documentation
    };
  } catch (error) {
    return {
      content: [{
        type: 'text' as const,
        text: `Error describing symbol: ${error instanceof Error ? error.message : 'Unknown error'}`
      }],
      isError: true
    };
  }
}
