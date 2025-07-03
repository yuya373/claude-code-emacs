import { z } from 'zod';

// Input schema for getCurrentSelection tool
export const getCurrentSelectionInputSchema = z.object({});

// Position schema
const positionSchema = z.object({
  line: z.number(),
  column: z.number()
});

// Output schema for getCurrentSelection tool
export const getCurrentSelectionOutputSchema = z.object({
  selection: z.string().optional(),
  file: z.string().optional(),
  start: positionSchema.optional(),
  end: positionSchema.optional()
});

// Inferred types from schemas
export type GetCurrentSelectionArgs = z.infer<typeof getCurrentSelectionInputSchema>;
export type GetCurrentSelectionResult = z.infer<typeof getCurrentSelectionOutputSchema>;
export type Position = z.infer<typeof positionSchema>;
