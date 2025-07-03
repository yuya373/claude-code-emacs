import { handleGetCurrentSelection } from '../../src/tools/selection-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

describe('selection-tools', () => {
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

  describe('handleGetCurrentSelection', () => {
    it('should return error when Emacs is not connected', async () => {
      mockBridge.isConnected.mockReturnValue(false);

      const result = await handleGetCurrentSelection(mockBridge, {});
      
      expect(result).toEqual({
        content: [{
          type: 'text',
          text: 'Error: Emacs is not connected'
        }],
        isError: true
      });
    });

    it('should return selected text information', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        text: 'selected text',
        startLine: 10,
        endLine: 12,
        startChar: 5,
        endChar: 15,
        fileName: 'test.js'
      });

      const result = await handleGetCurrentSelection(mockBridge, {});

      expect(mockBridge.request).toHaveBeenCalledWith('getCurrentSelection', {});
      expect(result.content[0].text).toContain('Selected text from test.js');
      expect(result.content[0].text).toContain('Lines 10-12');
      expect(result.content[0].text).toContain('columns 5-15');
      expect(result.content[0].text).toContain('selected text');
    });

    it('should handle single line selection', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        text: 'text',
        startLine: 5,
        endLine: 5,
        startChar: 10,
        endChar: 14,
        fileName: 'file.js'
      });

      const result = await handleGetCurrentSelection(mockBridge, {});

      expect(result.content[0].text).toContain('Line 5');
    });

    it('should handle no selection', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockResolvedValue({
        text: '',
        startLine: 0,
        endLine: 0,
        startChar: 0,
        endChar: 0,
        fileName: ''
      });

      const result = await handleGetCurrentSelection(mockBridge, {});

      expect(result.content[0].text).toBe('No text is currently selected');
    });

    it('should return error when request fails', async () => {
      mockBridge.isConnected.mockReturnValue(true);
      mockBridge.request.mockRejectedValue(new Error('Request failed'));

      const result = await handleGetCurrentSelection(mockBridge, {});
      
      expect(result).toEqual({
        content: [{
          type: 'text',
          text: 'Error getting current selection: Request failed'
        }],
        isError: true
      });
    });
  });
});