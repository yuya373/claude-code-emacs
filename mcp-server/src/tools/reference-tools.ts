import { EmacsBridge } from '../emacs-bridge.js';
import { FindReferencesArgs } from '../schemas/reference-schema.js';

interface EmacsReference {
  file: string;
  absolutePath: string;
  range: {
    start: { line: number; character: number };
    end: { line: number; character: number };
  };
  preview: string;
}

interface FindReferencesResult {
  references: EmacsReference[];
  count: number;
  error?: string;
}

interface ReferenceToolResult {
  content: Array<{ type: 'text'; text: string }>;
  references: Array<{
    file: string;
    line: number;
    column: number;
    preview: string;
  }>;
  isError?: boolean;
}

export async function handleFindReferences(bridge: EmacsBridge, args: FindReferencesArgs): Promise<ReferenceToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'Error: Emacs is not connected'
        }
      ],
      references: [],
      isError: true
    };
  }

  try {
    const result = await bridge.request('findReferences', {
      file: args.file,
      line: args.line,
      symbol: args.symbol,
      includeDeclaration: args.includeDeclaration ?? true
    }) as FindReferencesResult;

    if (result.error) {
      return {
        content: [
          {
            type: 'text' as const,
            text: `Error: ${result.error}`
          }
        ],
        references: [],
        isError: true
      };
    }

    // Transform references to match schema structure
    const structuredReferences = result.references.map(ref => ({
      file: ref.file,
      line: ref.range.start.line + 1, // Convert to 1-based
      column: ref.range.start.character + 1, // Convert to 1-based
      preview: ref.preview
    }));

    return {
      content: [
        {
          type: 'text' as const,
          text: formatReferencesResult(result)
        }
      ],
      references: structuredReferences
    };
  } catch (error) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Error finding references: ${error instanceof Error ? error.message : 'Unknown error'}`
        }
      ],
      references: [],
      isError: true
    };
  }
}

function formatReferencesResult(result: FindReferencesResult): string {
  if (result.references.length === 0) {
    return 'No references found';
  }

  const lines = [`Found ${result.count} reference${result.count === 1 ? '' : 's'}:\n`];

  // Group references by file
  const referencesByFile: Map<string, EmacsReference[]> = new Map();
  for (const ref of result.references) {
    const refs = referencesByFile.get(ref.file) || [];
    refs.push(ref);
    referencesByFile.set(ref.file, refs);
  }

  // Format each file's references
  for (const [file, refs] of referencesByFile) {
    lines.push(`📄 ${file}:`);
    for (const ref of refs) {
      const line = ref.range.start.line + 1; // Convert to 1-based
      const col = ref.range.start.character + 1; // Convert to 1-based
      lines.push(`  Line ${line}:${col} - ${ref.preview}`);
    }
    lines.push('');
  }

  return lines.join('\n');
}
