import { EmacsBridge } from '../emacs-bridge.js';

interface GetDiagnosticsArgs {
  buffer: string;  // Buffer name to execute lsp-diagnostics in (required for LSP context)
}

interface Diagnostic {
  file: string;
  line: number;
  column: number;
  severity: 'error' | 'warning' | 'info';
  message: string;
  source: string;
}

export { GetDiagnosticsArgs };

export async function handleGetDiagnostics(bridge: EmacsBridge, args: GetDiagnosticsArgs) {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  if (!args.buffer) {
    throw new Error('Buffer name is required for LSP context');
  }

  // Get project-wide diagnostics using the specified buffer's LSP context
  const result = await bridge.request('getDiagnostics', {
    buffer: args.buffer
  });
  const diagnostics: Diagnostic[] = result?.diagnostics || [];

  if (diagnostics.length === 0) {
    return {
      content: [
        {
          type: 'text',
          text: `No diagnostics found in project (executed from buffer ${args.buffer})`
        }
      ]
    };
  }

  // Group diagnostics by file
  const byFile = diagnostics.reduce((acc, diag) => {
    if (!acc[diag.file]) {
      acc[diag.file] = [];
    }
    acc[diag.file].push(diag);
    return acc;
  }, {} as Record<string, Diagnostic[]>);

  // Format output
  const output = Object.entries(byFile).map(([file, diags]) => {
    const fileSection = `## ${file}\n`;
    const diagList = diags.map(d =>
      `- **${d.severity.toUpperCase()}** [${d.source}] Line ${d.line}:${d.column}\n  ${d.message}`
    ).join('\n');
    return fileSection + diagList;
  }).join('\n\n');

  const summary = `Found ${diagnostics.length} diagnostic${diagnostics.length !== 1 ? 's' : ''} in ${Object.keys(byFile).length} file${Object.keys(byFile).length !== 1 ? 's' : ''}`;

  return {
    content: [
      {
        type: 'text',
        text: `${summary}\n\n${output}`
      }
    ]
  };
}
