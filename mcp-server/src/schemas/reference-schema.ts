import { z } from 'zod';

// Input schema for findReferences tool
export const findReferencesInputSchema = z.object({
  file: z.string().describe('File path to search from (required)'),
  line: z.number().describe('Line number (1-based, required)'),
  symbol: z.string().describe('Symbol name to search for (required)'),
  includeDeclaration: z.boolean().optional().describe('Include the declaration in results (default: true)')
});

// Reference location schema
const referenceSchema = z.object({
  file: z.string(),
  line: z.number(),
  column: z.number(),
  preview: z.string()
});

// Output schema for findReferences tool
export const findReferencesOutputSchema = z.object({
  references: z.array(referenceSchema)
});

// Inferred types from schemas
export type FindReferencesArgs = z.infer<typeof findReferencesInputSchema>;
export type FindReferencesResult = z.infer<typeof findReferencesOutputSchema>;
export type Reference = z.infer<typeof referenceSchema>;