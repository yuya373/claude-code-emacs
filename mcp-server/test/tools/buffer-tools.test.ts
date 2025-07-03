import { handleGetOpenBuffers } from '../../src/tools/buffer-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

describe('buffer-tools', () => {
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

  describe('handleGetOpenBuffers', () => {
    it('should return error when Emacs is not connected', async () => {
      mockBridge.isConnected.mockReturnValue(false);

      const result = await handleGetOpenBuffers(mockBridge, {});
      
      expect(result).toEqual({
        content: [{
          type: 'text',
          text: 'Error: Emacs is not connected'
        }],
        buffers: [],
        isError: true
      });
    });

    it('should return buffer list when buffers are open', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        buffers: [
          {
            path: '/project/file1.js',
            name: 'file1.js',
            active: true,
            modified: false
          },
          {
            path: '/project/file2.js',
            name: 'file2.js',
            active: false,
            modified: true
          }
        ]
      });

      const result = await handleGetOpenBuffers(mockBridge, {});

      expect(mockBridge.request).toHaveBeenCalledWith('getOpenBuffers', {});
      expect(result.content[0].text).toContain('Open buffers (2)');
      expect(result.content[0].text).toContain('* file1.js');
      expect(result.content[0].text).toContain('file2.js [modified]');
    });

    it('should handle empty buffer list', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        buffers: []
      });

      const result = await handleGetOpenBuffers(mockBridge, {});

      expect(result.content[0].text).toBe('No open buffers in current project');
    });

    it('should pass includeHidden parameter', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        buffers: []
      });

      await handleGetOpenBuffers(mockBridge, { includeHidden: true });

      expect(mockBridge.request).toHaveBeenCalledWith('getOpenBuffers', { includeHidden: true });
    });

    it('should return error when request fails', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockRejectedValue(new Error('Request failed'));

      const result = await handleGetOpenBuffers(mockBridge, {});
      
      expect(result).toEqual({
        content: [{
          type: 'text',
          text: 'Error getting open buffers: Request failed'
        }],
        buffers: [],
        isError: true
      });
    });
  });
});