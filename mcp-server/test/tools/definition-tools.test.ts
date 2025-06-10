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
          symbol: 'my-function',
          preview: '(defun my-function (arg)\n  "Documentation string."\n  (message "Hello %s" arg))',
          range: {
            start: { line: 41, character: 9 },
            end: { line: 41, character: 20 }
          }
        }
      ],
      searchedSymbol: 'my-function',
      method: 'lsp'
    });

    const args: GetDefinitionArgs = {
      file: '/path/to/current.el',
      line: 10,
      symbol: 'my-function'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toContain('Found 1 definition using lsp');
    expect(result.content[0].text).toContain('my-function');
    expect(result.content[0].text).toContain('/path/to/file.el');
    expect(result.content[0].text).toContain('Line 42, Column 10');
    expect(result.content[0].text).toContain('(defun my-function');
  });

  it('should handle multiple definitions via LSP', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      definitions: [
        {
          file: '/path/to/file1.ts',
          symbol: 'MyClass',
          preview: 'export class MyClass {\n  constructor() {}\n}',
          range: {
            start: { line: 9, character: 4 },
            end: { line: 9, character: 11 }
          }
        },
        {
          file: '/path/to/file2.ts',
          symbol: 'MyClass',
          preview: 'class MyClass extends BaseClass {\n  // implementation\n}',
          range: {
            start: { line: 19, character: 7 },
            end: { line: 19, character: 14 }
          }
        }
      ],
      searchedSymbol: 'MyClass',
      method: 'lsp'
    });

    const args: GetDefinitionArgs = {
      file: '/path/to/usage.ts',
      line: 5,
      symbol: 'MyClass'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toContain('Found 2 definitions using lsp');
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
      method: 'lsp'
    });

    const args: GetDefinitionArgs = {
      file: '/path/to/current.el',
      line: 20,
      symbol: 'unknownSymbol'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toBe('No definition found for unknownSymbol');
  });

  it('should throw error if Emacs not connected', async () => {
    mockBridge.isConnected.mockReturnValue(false);

    const args: GetDefinitionArgs = {
      file: '/path/to/current.el',
      line: 1,
      symbol: 'test'
    };

    await expect(handleGetDefinition(mockBridge, args))
      .rejects.toThrow('Emacs is not connected');
  });

  // This test is no longer valid since file is required by TypeScript
  // The TypeScript compiler will enforce the file parameter at compile time

  it('should handle Emacs errors gracefully', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockRejectedValue(new Error('LSP not available'));

    const args: GetDefinitionArgs = {
      file: '/path/to/current.el',
      line: 1,
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
          symbol: 'calculate',
          // No preview field
          range: {
            start: { line: 14, character: 0 },
            end: { line: 14, character: 9 }
          }
        }
      ],
      searchedSymbol: 'calculate',
      method: 'lsp'
    });

    const args: GetDefinitionArgs = {
      file: '/path/to/current.py',
      line: 15,
      symbol: 'calculate'
    };

    const result = await handleGetDefinition(mockBridge, args);

    expect(result.content[0].text).toContain('calculate');
    expect(result.content[0].text).not.toContain('```'); // No code block
  });
});