import { z } from 'zod';

// Input schema for getDefinition tool
export const getDefinitionInputSchema = z.object({
  file: z.string().describe('File path, absolute or relative to the project root'),
  line: z.number().describe('1-based line number where symbol appears'),
  symbol: z.string().describe('Symbol name exactly as written on that line')
});

// Definition location schema
const definitionSchema = z.object({
  file: z.string(),
  line: z.number(),
  column: z.number(),
  preview: z.string().optional()
});

// Output schema for getDefinition tool
export const getDefinitionOutputSchema = z.object({
  definitions: z.array(definitionSchema)
});

// Inferred types from schemas
export type GetDefinitionArgs = z.infer<typeof getDefinitionInputSchema>;
export type GetDefinitionResult = z.infer<typeof getDefinitionOutputSchema>;
export type Definition = z.infer<typeof definitionSchema>;