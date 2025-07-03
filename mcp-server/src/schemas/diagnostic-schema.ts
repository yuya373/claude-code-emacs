import { z } from 'zod';

// Input schema for getDiagnostics tool
export const getDiagnosticsInputSchema = z.object({
  buffer: z.string().describe('Buffer name to execute lsp-diagnostics in (required for LSP workspace context)')
});

// LSP diagnostic schema
const diagnosticSchema = z.object({
  range: z.object({
    start: z.object({ line: z.number(), character: z.number() }),
    end: z.object({ line: z.number(), character: z.number() })
  }),
  severity: z.string(),
  message: z.string(),
  source: z.string().optional(),
  code: z.union([z.string(), z.number()]).optional()
});

// Output schema for getDiagnostics tool
export const getDiagnosticsOutputSchema = z.object({
  diagnostics: z.array(z.object({
    file: z.string(),
    diagnostics: z.array(diagnosticSchema)
  }))
});

// Inferred types from schemas
export type GetDiagnosticsArgs = z.infer<typeof getDiagnosticsInputSchema>;
export type GetDiagnosticsResult = z.infer<typeof getDiagnosticsOutputSchema>;
export type Diagnostic = z.infer<typeof diagnosticSchema>;
