import { EmacsBridge } from '../emacs-bridge.js';
import { GetDiagnosticsArgs } from '../schemas/diagnostic-schema.js';

// Emacs returns diagnostics with a different structure
interface EmacsDiagnostic {
  file: string;
  line: number;
  column: number;
  severity: 'error' | 'warning' | 'info';
  message: string;
  source: string;
}

interface DiagnosticToolResult {
  content: Array<{ type: 'text'; text: string }>;
  diagnostics: Array<{
    file: string;
    diagnostics: Array<{
      range: {
        start: { line: number; character: number };
        end: { line: number; character: number };
      };
      severity: string;
      message: string;
      source?: string;
      code?: string | number;
    }>;
  }>;
  isError?: boolean;
}

export async function handleGetDiagnostics(bridge: EmacsBridge, args: GetDiagnosticsArgs): Promise<DiagnosticToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'Error: Emacs is not connected'
        }
      ],
      diagnostics: [],
      isError: true
    };
  }

  try {
    // Get project-wide diagnostics using the specified buffer's LSP context
    const result = await bridge.request('getDiagnostics', {
      buffer: args.buffer
    });
    const diagnostics: EmacsDiagnostic[] = result?.diagnostics || [];

    if (diagnostics.length === 0) {
      return {
        content: [
          {
            type: 'text' as const,
            text: `No diagnostics found in project (executed from buffer ${args.buffer})`
          }
        ],
        diagnostics: []
      };
    }

    // Group diagnostics by file
    const byFile = diagnostics.reduce((acc, diag) => {
      if (!acc[diag.file]) {
        acc[diag.file] = [];
      }
      acc[diag.file].push(diag);
      return acc;
    }, {} as Record<string, EmacsDiagnostic[]>);

    // Format output
    const output = Object.entries(byFile).map(([file, diags]) => {
      const fileSection = `## ${file}\n`;
      const diagList = diags.map(d =>
        `- **${d.severity.toUpperCase()}** [${d.source}] Line ${d.line}:${d.column}\n  ${d.message}`
      ).join('\n');
      return fileSection + diagList;
    }).join('\n\n');

    const summary = `Found ${diagnostics.length} diagnostic${diagnostics.length !== 1 ? 's' : ''} in ${Object.keys(byFile).length} file${Object.keys(byFile).length !== 1 ? 's' : ''}`;

    // Transform diagnostics to match schema structure
    const structuredDiagnostics = Object.entries(byFile).map(([file, diags]) => ({
      file,
      diagnostics: diags.map(d => ({
        range: {
          start: { line: d.line - 1, character: d.column }, // Convert to 0-based
          end: { line: d.line - 1, character: d.column } // Single point for now
        },
        severity: d.severity,
        message: d.message,
        source: d.source
      }))
    }));

    return {
      content: [
        {
          type: 'text' as const,
          text: `${summary}\n\n${output}`
        }
      ],
      diagnostics: structuredDiagnostics
    };
  } catch (error) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Error getting diagnostics: ${error instanceof Error ? error.message : 'Unknown error'}`
        }
      ],
      diagnostics: [],
      isError: true
    };
  }
}
