import { z } from 'zod';

// Common output schema for all diff tools
export const diffToolOutputSchema = z.object({
  status: z.enum(['success', 'error']),
  message: z.string(),
  file: z.string().optional()
});

// openDiffFile schemas
export const openDiffFileInputSchema = z.object({
  fileA: z.string().describe('First file, absolute or relative to the project root'),
  fileB: z.string().describe('Second file, absolute or relative to the project root')
});

// openRevisionDiff schemas
export const openRevisionDiffInputSchema = z.object({
  file: z.string().describe('File to compare with its git history'),
  revision: z.string().default('HEAD').describe('Git revision to compare against (e.g., HEAD, HEAD~1, branch-name, commit-hash)')
});

// openCurrentChanges schemas
export const openCurrentChangesInputSchema = z.object({
  file: z.string().optional().describe('File, absolute or relative to the project root; defaults to the file of the current Emacs buffer')
});

// openDiffContent schemas
export const openDiffContentInputSchema = z.object({
  contentA: z.string().describe('First text content to compare'),
  contentB: z.string().describe('Second text content to compare'),
  titleA: z.string().describe('Buffer name for the first content (e.g., "*Original Code*"); an existing buffer with this name is overwritten'),
  titleB: z.string().describe('Buffer name for the second content (e.g., "*Modified Code*"); must differ from titleA, otherwise both contents land in one buffer')
});

// Inferred types from schemas
export type DiffToolResult = z.infer<typeof diffToolOutputSchema>;
export type OpenDiffFileArgs = z.infer<typeof openDiffFileInputSchema>;
export type OpenRevisionDiffArgs = z.infer<typeof openRevisionDiffInputSchema>;
export type OpenCurrentChangesArgs = z.infer<typeof openCurrentChangesInputSchema>;
export type OpenDiffContentArgs = z.infer<typeof openDiffContentInputSchema>;
