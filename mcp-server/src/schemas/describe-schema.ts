import { z } from 'zod';

// Input schema for describeSymbol tool
export const describeSymbolInputSchema = z.object({
  file: z.string().describe('File path, absolute or relative to the project root'),
  line: z.number().describe('1-based line number where symbol appears'),
  symbol: z.string().describe('Symbol name exactly as written on that line')
});

// Output schema for describeSymbol tool
export const describeSymbolOutputSchema = z.object({
  documentation: z.string().optional()
});

// Inferred types from schemas
export type DescribeSymbolArgs = z.infer<typeof describeSymbolInputSchema>;
export type DescribeSymbolResult = z.infer<typeof describeSymbolOutputSchema>;
