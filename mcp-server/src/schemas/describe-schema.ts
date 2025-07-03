import { z } from 'zod';

// Input schema for describeSymbol tool
export const describeSymbolInputSchema = z.object({
  file: z.string().describe('File path relative to project root'),
  line: z.number().describe('Line number (1-based)'),
  symbol: z.string().describe('Symbol name to describe')
});

// Output schema for describeSymbol tool
export const describeSymbolOutputSchema = z.object({
  documentation: z.string().optional()
});

// Inferred types from schemas
export type DescribeSymbolArgs = z.infer<typeof describeSymbolInputSchema>;
export type DescribeSymbolResult = z.infer<typeof describeSymbolOutputSchema>;
