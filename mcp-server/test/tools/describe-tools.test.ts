import { handleDescribeSymbol, DescribeSymbolArgs } from '../../src/tools/describe-tools.js';
import { EmacsBridge } from '../../src/emacs-bridge.js';
import { jest } from '@jest/globals';

describe('handleDescribeSymbol', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn(),
      request: jest.fn()
    } as any;
  });

  it('should describe symbol successfully with documentation', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      description: {
        documentation: 'Calculates the total price of all items in the array.\n\n@param items - Array of items with price property\n@returns The sum of all item prices'
      },
      method: 'lsp'
    });

    const args: DescribeSymbolArgs = {
      file: 'src/utils.ts',
      line: 100,
      symbol: 'calculateTotal'
    };

    const result = await handleDescribeSymbol(mockBridge, args);

    expect(result.content[0].text).toBe('Calculates the total price of all items in the array.\n\n@param items - Array of items with price property\n@returns The sum of all item prices');
  });

  // This test is now redundant with the first test, so we can remove it

  it('should handle no description found', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      description: null,
      method: 'lsp'
    });

    const args: DescribeSymbolArgs = {
      file: 'src/unknown.ts',
      line: 10,
      symbol: 'unknownSymbol'
    };

    const result = await handleDescribeSymbol(mockBridge, args);

    expect(result.content[0].text).toBe('No documentation found for unknownSymbol');
  });

  it('should throw error if Emacs not connected', async () => {
    mockBridge.isConnected.mockReturnValue(false);

    const args: DescribeSymbolArgs = {
      file: 'src/test.ts',
      line: 1,
      symbol: 'test'
    };

    await expect(handleDescribeSymbol(mockBridge, args))
      .rejects.toThrow('Emacs is not connected');
  });

  it('should validate required parameters', async () => {
    mockBridge.isConnected.mockReturnValue(true);

    // Missing file
    await expect(handleDescribeSymbol(mockBridge, {
      line: 1,
      symbol: 'test'
    } as any)).rejects.toThrow('file parameter is required');

    // Missing line
    await expect(handleDescribeSymbol(mockBridge, {
      file: 'test.ts',
      symbol: 'test'
    } as any)).rejects.toThrow('line parameter is required');

    // Missing symbol
    await expect(handleDescribeSymbol(mockBridge, {
      file: 'test.ts',
      line: 1
    } as any)).rejects.toThrow('symbol parameter is required');
  });

  it('should handle Emacs errors gracefully', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockRejectedValue(new Error('LSP not available'));

    const args: DescribeSymbolArgs = {
      file: 'src/test.ts',
      line: 1,
      symbol: 'test'
    };

    await expect(handleDescribeSymbol(mockBridge, args))
      .rejects.toThrow('Failed to describe symbol: LSP not available');
  });

  // This test is also no longer needed
});