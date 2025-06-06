import { EmacsBridge } from '../emacs-bridge.js';

// Empty interface as we no longer accept parameters
interface GetDiagnosticsArgs {}

interface Diagnostic {
  file: string;
  line: number;
  column: number;
  severity: 'error' | 'warning' | 'info';
  message: string;
  source: string;
}

export async function handleGetDiagnostics(bridge: EmacsBridge, _args: GetDiagnosticsArgs) {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  // Always get project-wide diagnostics
  const result = await bridge.request('getDiagnostics', {});
  const diagnostics: Diagnostic[] = result?.diagnostics || [];

  if (diagnostics.length === 0) {
    return {
      content: [
        {
          type: 'text',
          text: 'No diagnostics found in current project'
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
