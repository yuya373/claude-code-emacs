import { handleGetDefinition, GetDefinitionArgs } from '../../src/tools/definition-tools.js';
import { EmacsBridge } from '../../src/emacs-bridge.js';
import { jest } from '@jest/globals';

describe('handleGetDefinition', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn(),
      request: jest.fn()
    } as any;
  });

  it('should handle definition found via LSP', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      definitions: [
        {
          file: '/path/to/file.el',
          line: 42,
          column: 10,
          symbol: 'my-function',
          type: 'function',
          preview: '(defun my-function (arg)\n  "Documentation string."\n  (message "Hello %s" arg))'
        }
      ],
      searchedSymbol: 'my-function',
      method: 'lsp'
    });

    const args: GetDefinitionArgs = {
      symbol: 'my-function'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toContain('Found 1 definition using lsp');
    expect(result.content[0].text).toContain('my-function');
    expect(result.content[0].text).toContain('/path/to/file.el');
    expect(result.content[0].text).toContain('Line 42, Column 10');
    expect(result.content[0].text).toContain('(defun my-function');
  });

  it('should handle multiple definitions via xref', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      definitions: [
        {
          file: '/path/to/file1.ts',
          line: 10,
          column: 5,
          symbol: 'MyClass',
          type: 'class',
          preview: 'export class MyClass {\n  constructor() {}\n}'
        },
        {
          file: '/path/to/file2.ts',
          line: 20,
          column: 8,
          symbol: 'MyClass',
          type: 'class',
          preview: 'class MyClass extends BaseClass {\n  // implementation\n}'
        }
      ],
      searchedSymbol: 'MyClass',
      method: 'xref'
    });

    const args: GetDefinitionArgs = {
      file: '/path/to/usage.ts',
      line: 5,
      column: 10
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toContain('Found 2 definitions using xref');
    expect(result.content[0].text).toContain('Definition 1');
    expect(result.content[0].text).toContain('Definition 2');
    expect(result.content[0].text).toContain('/path/to/file1.ts');
    expect(result.content[0].text).toContain('/path/to/file2.ts');
  });

  it('should handle no definition found', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      definitions: [],
      searchedSymbol: 'unknownSymbol',
      method: 'xref'
    });

    const args: GetDefinitionArgs = {
      symbol: 'unknownSymbol'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toBe('No definition found for unknownSymbol');
  });

  it('should throw error if Emacs not connected', async () => {
    mockBridge.isConnected.mockReturnValue(false);

    const args: GetDefinitionArgs = {
      symbol: 'test'
    };

    await expect(handleGetDefinition(mockBridge, args))
      .rejects.toThrow('Emacs is not connected');
  });

  it('should throw error if no symbol or file/line provided', async () => {
    mockBridge.isConnected.mockReturnValue(true);

    const args: GetDefinitionArgs = {};

    await expect(handleGetDefinition(mockBridge, args))
      .rejects.toThrow('Either symbol or file/line must be provided');
  });

  it('should handle Emacs errors gracefully', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockRejectedValue(new Error('LSP not available'));

    const args: GetDefinitionArgs = {
      symbol: 'test'
    };

    await expect(handleGetDefinition(mockBridge, args))
      .rejects.toThrow('Failed to get definition: LSP not available');
  });

  it('should format definition without preview', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      definitions: [
        {
          file: '/path/to/file.py',
          line: 15,
          column: 0,
          symbol: 'calculate',
          type: 'function'
          // No preview field
        }
      ],
      searchedSymbol: 'calculate',
      method: 'lsp'
    });

    const args: GetDefinitionArgs = {
      symbol: 'calculate'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toContain('calculate (function)');
    expect(result.content[0].text).not.toContain('```'); // No code block
  });
});