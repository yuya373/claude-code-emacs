import { z } from 'zod';

// Input schema for getOpenBuffers tool
export const getOpenBuffersInputSchema = z.object({
  includeHidden: z.boolean().optional().describe('Include hidden buffers (starting with space)')
});

// Buffer schema
const bufferSchema = z.object({
  path: z.string(),
  name: z.string(),
  active: z.boolean(),
  modified: z.boolean()
});

// Output schema for getOpenBuffers tool
export const getOpenBuffersOutputSchema = z.object({
  buffers: z.array(bufferSchema)
});

// Inferred types from schemas
export type GetOpenBuffersArgs = z.infer<typeof getOpenBuffersInputSchema>;
export type GetOpenBuffersResult = z.infer<typeof getOpenBuffersOutputSchema>;
export type Buffer = z.infer<typeof bufferSchema>;