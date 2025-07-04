import { z } from 'zod';

// Common output schema for all diff tools
export const diffToolOutputSchema = z.object({
  status: z.enum(['success', 'error']),
  message: z.string(),
  file: z.string().optional()
});

// openDiff schemas
export const openDiffInputSchema = z.object({
  fileA: z.string().describe('Path to the first file to compare (must be different from fileB)'),
  fileB: z.string().describe('Path to the second file to compare (must be different from fileA)')
});

// openRevisionDiff schemas
export const openRevisionDiffInputSchema = z.object({
  file: z.string().describe('File to compare with its git history'),
  revision: z.string().default('HEAD').describe('Git revision to compare against (e.g., HEAD, HEAD~1, branch-name, commit-hash)')
});

// openCurrentChanges schemas
export const openCurrentChangesInputSchema = z.object({
  file: z.string().optional().describe('File to show uncommitted changes for (optional, defaults to current file)')
});

// openDiffContent schemas
export const openDiffContentInputSchema = z.object({
  contentA: z.string().describe('First text content to compare'),
  contentB: z.string().describe('Second text content to compare (should be different from contentA)'),
  titleA: z.string().describe('Descriptive title for the first content (e.g., "Original Code", "Version 1")'),
  titleB: z.string().describe('Descriptive title for the second content (e.g., "Modified Code", "Version 2")')
});

// Inferred types from schemas
export type DiffToolResult = z.infer<typeof diffToolOutputSchema>;
export type OpenDiffArgs = z.infer<typeof openDiffInputSchema>;
export type OpenRevisionDiffArgs = z.infer<typeof openRevisionDiffInputSchema>;
export type OpenCurrentChangesArgs = z.infer<typeof openCurrentChangesInputSchema>;
export type OpenDiffContentArgs = z.infer<typeof openDiffContentInputSchema>;
