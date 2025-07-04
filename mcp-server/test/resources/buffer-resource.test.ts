import { describe, it, expect, jest, beforeEach } from '@jest/globals';
import { bufferResourceHandler } from '../../src/resources/buffer-resource.js';
import { EmacsBridge } from '../../src/emacs-bridge.js';

describe('bufferResourceHandler', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      sendRequest: jest.fn()
    } as any;
  });

  describe('list', () => {
    it('should list buffer resources', async () => {
      mockBridge.sendRequest.mockResolvedValue({
        buffers: [
          { path: '/project/src/main.ts', name: 'main.ts' },
          { path: '/project/test/main.test.ts', name: 'main.test.ts' }
        ]
      });

      const resources = await bufferResourceHandler.list(mockBridge);

      expect(resources).toHaveLength(2);
      expect(resources[0]).toEqual({
        uri: 'emacs://buffer//project/src/main.ts',
        name: 'main.ts',
        description: 'Buffer: /project/src/main.ts',
        mimeType: 'text/typescript'
      });
    });

    it('should handle empty buffer list', async () => {
      mockBridge.sendRequest.mockResolvedValue({
        buffers: []
      });

      const resources = await bufferResourceHandler.list(mockBridge);
      expect(resources).toEqual([]);
    });

    it('should handle errors gracefully', async () => {
      mockBridge.sendRequest.mockRejectedValue(new Error('Connection failed'));

      const resources = await bufferResourceHandler.list(mockBridge);
      expect(resources).toEqual([]);
    });
  });

  describe('read', () => {
    it('should read buffer content', async () => {
      mockBridge.sendRequest.mockResolvedValue({
        content: 'const hello = "world";'
      });

      const result = await bufferResourceHandler.read(mockBridge, 'emacs://buffer//project/src/main.ts');

      expect(mockBridge.sendRequest).toHaveBeenCalledWith('get-buffer-content', {
        path: '/project/src/main.ts'
      });
      expect(result).toEqual({
        uri: 'emacs://buffer//project/src/main.ts',
        mimeType: 'text/typescript',
        text: 'const hello = "world";'
      });
    });

    it('should throw error on failure', async () => {
      mockBridge.sendRequest.mockResolvedValue({
        error: 'File not found'
      });

      await expect(
        bufferResourceHandler.read(mockBridge, 'emacs://buffer//project/missing.ts')
      ).rejects.toThrow('Failed to read buffer resource: Error: File not found');
    });
  });
});