import { handleGetDiagnostics } from '../../src/tools/diagnostic-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

describe('diagnostic-tools', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn(),
      request: jest.fn(),
      start: jest.fn(),
      stop: jest.fn(),
      on: jest.fn(),
      emit: jest.fn(),
    } as any;
  });

  describe('handleGetDiagnostics', () => {
    it('should throw error when Emacs is not connected', async () => {
      mockBridge.isConnected.mockReturnValue(false);

      await expect(handleGetDiagnostics(mockBridge, {}))
        .rejects.toThrow('Emacs is not connected');
    });

    it('should return diagnostics grouped by file', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        diagnostics: [
          {
            file: '/project/file1.js',
            line: 10,
            column: 5,
            severity: 'error',
            message: 'Undefined variable',
            source: 'eslint'
          },
          {
            file: '/project/file1.js',
            line: 20,
            column: 10,
            severity: 'warning',
            message: 'Unused variable',
            source: 'eslint'
          },
          {
            file: '/project/file2.js',
            line: 5,
            column: 1,
            severity: 'info',
            message: 'Consider using const',
            source: 'typescript'
          }
        ]
      });

      const result = await handleGetDiagnostics(mockBridge, {});

      expect(mockBridge.request).toHaveBeenCalledWith('getDiagnostics', {});
      expect(result.content[0].text).toContain('Found 3 diagnostics in 2 files');
      expect(result.content[0].text).toContain('## /project/file1.js');
      expect(result.content[0].text).toContain('## /project/file2.js');
      expect(result.content[0].text).toContain('**ERROR** [eslint] Line 10:5');
      expect(result.content[0].text).toContain('**WARNING** [eslint] Line 20:10');
      expect(result.content[0].text).toContain('**INFO** [typescript] Line 5:1');
    });

    it('should handle no diagnostics', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        diagnostics: []
      });

      const result = await handleGetDiagnostics(mockBridge, {});

      expect(result.content[0].text).toBe('No diagnostics found in current project');
    });

    it('should pass buffer path parameter', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        diagnostics: []
      });

      const result = await handleGetDiagnostics(mockBridge, { bufferPath: '/project/specific.js' });

      expect(mockBridge.request).toHaveBeenCalledWith('getDiagnostics', { bufferPath: '/project/specific.js' });
      expect(result.content[0].text).toBe('No diagnostics found for /project/specific.js');
    });

    it('should handle single diagnostic', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        diagnostics: [
          {
            file: '/project/file.js',
            line: 1,
            column: 1,
            severity: 'error',
            message: 'Test error',
            source: 'test'
          }
        ]
      });

      const result = await handleGetDiagnostics(mockBridge, {});

      expect(result.content[0].text).toContain('Found 1 diagnostic in 1 file');
    });
  });
});
