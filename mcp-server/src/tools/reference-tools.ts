import { EmacsBridge } from '../emacs-bridge.js';

export interface FindReferencesArgs {
  file: string;               // File path relative to project root
  line: number;               // Line number (1-based)
  symbol: string;             // Symbol name to search for
  includeDeclaration?: boolean; // Include the declaration in results
}

interface Reference {
  file: string;
  absolutePath: string;
  range: {
    start: { line: number; character: number };
    end: { line: number; character: number };
  };
  preview: string;
}

interface FindReferencesResult {
  references: Reference[];
  count: number;
  error?: string;
}

export async function handleFindReferences(bridge: EmacsBridge, args: FindReferencesArgs): Promise<any> {
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

  const result = await bridge.request('findReferences', {
    file: args.file,
    line: args.line,
    symbol: args.symbol,
    includeDeclaration: args.includeDeclaration ?? true
  }) as FindReferencesResult;

  if (result.error) {
    throw new Error(result.error);
  }

  return {
    content: [
      {
        type: 'text',
        text: formatReferencesResult(result)
      }
    ]
  };
}

function formatReferencesResult(result: FindReferencesResult): string {
  if (result.references.length === 0) {
    return 'No references found';
  }

  const lines = [`Found ${result.count} reference${result.count === 1 ? '' : 's'}:\n`];

  // Group references by file
  const referencesByFile: Map<string, Reference[]> = new Map();
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
