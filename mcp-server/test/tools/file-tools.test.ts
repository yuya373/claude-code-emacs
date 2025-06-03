import { handleOpenFile } from '../../src/tools/file-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

describe('file-tools', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn(),
      request: jest.fn(),
      start: jest.fn(),
      stop: jest.fn(),
      on: jest.fn(),
      emit: jest.fn(),
      // Add other EventEmitter methods as needed
    } as any;
  });

  describe('handleOpenFile', () => {
    it('should throw error when Emacs is not connected', async () => {
      mockBridge.isConnected.mockReturnValue(false);

      await expect(handleOpenFile(mockBridge, { path: 'test.js' }))
        .rejects.toThrow('Emacs is not connected');
    });

    it('should throw error when path is not provided', async () => {
      mockBridge.isConnected.mockReturnValue(true);

      await expect(handleOpenFile(mockBridge, {}))
        .rejects.toThrow('File path is required');
    });

    it('should return success message when file is opened', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        success: true,
        path: '/project/test.js'
      });

      const result = await handleOpenFile(mockBridge, { path: 'test.js' });

      expect(mockBridge.request).toHaveBeenCalledWith('openFile', { path: 'test.js' });
      expect(result).toEqual({
        content: [{
          type: 'text',
          text: 'Opened file: /project/test.js'
        }]
      });
    });

    it('should include text selection info in message', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        success: true,
        path: '/project/test.js'
      });

      const result = await handleOpenFile(mockBridge, {
        path: 'test.js',
        startText: 'function',
        endText: '}'
      });

      expect(result.content[0].text).toContain('with text selection');
    });

    it('should return failure message when file cannot be opened', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        success: false,
        path: '/project/test.js'
      });

      const result = await handleOpenFile(mockBridge, { path: 'test.js' });

      expect(result.content[0].text).toBe('Failed to open file: test.js');
    });
  });
});