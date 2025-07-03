import { describe, it, expect, jest, beforeEach } from '@jest/globals';
import { handleFindReferences } from '../../src/tools/reference-tools.js';
import { EmacsBridge } from '../../src/emacs-bridge.js';

describe('handleFindReferences', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn().mockReturnValue(true),
      request: jest.fn()
    } as any;
  });

  it('should find references successfully', async () => {
    const mockReferences = {
      references: [
        {
          file: 'src/main.ts',
          absolutePath: '/project/src/main.ts',
          range: {
            start: { line: 10, character: 5 },
            end: { line: 10, character: 15 }
          },
          preview: '  const myFunction = () => {'
        },
        {
          file: 'src/utils.ts',
          absolutePath: '/project/src/utils.ts',
          range: {
            start: { line: 20, character: 10 },
            end: { line: 20, character: 20 }
          },
          preview: '    myFunction();'
        }
      ],
      count: 2
    };

    mockBridge.request.mockResolvedValue(mockReferences);

    const result = await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction',
      includeDeclaration: true
    });

    expect(mockBridge.request).toHaveBeenCalledWith('findReferences', {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction',
      includeDeclaration: true
    });

    expect(result.content[0].text).toContain('Found 2 references');
    expect(result.content[0].text).toContain('src/main.ts');
    expect(result.content[0].text).toContain('src/utils.ts');
  });

  it('should handle no references found', async () => {
    mockBridge.request.mockResolvedValue({
      references: [],
      count: 0
    });

    const result = await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction'
    });

    expect(result.content[0].text).toBe('No references found');
  });

  it('should handle default includeDeclaration', async () => {
    mockBridge.request.mockResolvedValue({
      references: [],
      count: 0
    });

    await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction'
    });

    expect(mockBridge.request).toHaveBeenCalledWith('findReferences', {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction',
      includeDeclaration: true
    });
  });

  it('should return error when Emacs is not connected', async () => {
    mockBridge.isConnected.mockReturnValue(false);

    const result = await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction'
    });
    
    expect(result).toEqual({
      content: [{
        type: 'text',
        text: 'Error: Emacs is not connected'
      }],
      references: [],
      isError: true
    });
  });

  it('should handle error from Emacs', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockResolvedValue({
      error: 'LSP is not active in this buffer'
    });

    const result = await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction'
    });
    
    expect(result).toEqual({
      content: [{
        type: 'text',
        text: 'Error: LSP is not active in this buffer'
      }],
      references: [],
      isError: true
    });
  });

  it('should format references grouped by file', async () => {
    const mockReferences = {
      references: [
        {
          file: 'src/main.ts',
          absolutePath: '/project/src/main.ts',
          range: {
            start: { line: 10, character: 5 },
            end: { line: 10, character: 15 }
          },
          preview: '  const myFunction = () => {'
        },
        {
          file: 'src/main.ts',
          absolutePath: '/project/src/main.ts',
          range: {
            start: { line: 25, character: 8 },
            end: { line: 25, character: 18 }
          },
          preview: '  return myFunction;'
        },
        {
          file: 'src/utils.ts',
          absolutePath: '/project/src/utils.ts',
          range: {
            start: { line: 20, character: 10 },
            end: { line: 20, character: 20 }
          },
          preview: '    myFunction();'
        }
      ],
      count: 3
    };

    mockBridge.request.mockResolvedValue(mockReferences);

    const result = await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction'
    });

    const text = result.content[0].text;
    expect(text).toContain('Found 3 references');
    expect(text).toContain('📄 src/main.ts:');
    expect(text).toContain('Line 11:6');  // 1-based line and column
    expect(text).toContain('Line 26:9');  // 1-based line and column
    expect(text).toContain('📄 src/utils.ts:');
    expect(text).toContain('Line 21:11'); // 1-based line and column
  });

  it('should return error when request fails', async () => {
    mockBridge.isConnected.mockReturnValue(true);
    mockBridge.request.mockRejectedValue(new Error('Network error'));

    const result = await handleFindReferences(mockBridge, {
      file: 'src/main.ts',
      line: 10,
      symbol: 'myFunction'
    });
    
    expect(result).toEqual({
      content: [{
        type: 'text',
        text: 'Error finding references: Network error'
      }],
      references: [],
      isError: true
    });
  });
});