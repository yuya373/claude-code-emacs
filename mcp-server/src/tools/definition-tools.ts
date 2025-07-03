import { EmacsBridge } from '../emacs-bridge.js';
import { GetDefinitionArgs, Definition } from '../schemas/definition-schema.js';

export { GetDefinitionArgs };

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

interface DefinitionToolResult {
  content: Array<{ type: 'text'; text: string }>;
  definitions: Array<{
    file: string;
    line: number;
    column: number;
    preview?: string;
  }>;
  isError?: boolean;
}

export async function handleGetDefinition(bridge: EmacsBridge, args: GetDefinitionArgs): Promise<DefinitionToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [{
        type: 'text' as const,
        text: 'Error: Emacs is not connected'
      }],
      definitions: [],
      isError: true
    };
  }

  try {
    const result = await bridge.request('getDefinition', args) as DefinitionResult;

    if (!result.definitions || result.definitions.length === 0) {
      return {
        content: [{
          type: 'text' as const,
          text: `No definition found for ${args.symbol}`
        }],
        definitions: []
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

    // Transform definitions to match schema structure
    const structuredDefinitions = result.definitions.map(def => ({
      file: def.file,
      line: def.range.start.line + 1, // Convert to 1-based
      column: def.range.start.character + 1, // Convert to 1-based
      preview: def.preview
    }));

    return {
      content: [{
        type: 'text' as const,
        text: output.trim()
      }],
      definitions: structuredDefinitions
    };
  } catch (error) {
    return {
      content: [{
        type: 'text' as const,
        text: `Error getting definition: ${error instanceof Error ? error.message : 'Unknown error'}`
      }],
      definitions: [],
      isError: true
    };
  }
}
