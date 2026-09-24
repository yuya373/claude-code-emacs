import { z } from 'zod';

// Input schema for findReferences tool
export const findReferencesInputSchema = z.object({
  file: z.string().describe('File path, absolute or relative to the project root'),
  line: z.number().describe('1-based line number where symbol appears'),
  symbol: z.string().describe('Symbol name exactly as written on that line'),
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