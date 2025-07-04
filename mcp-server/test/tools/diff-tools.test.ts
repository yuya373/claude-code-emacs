import { diffTools } from '../../src/tools/diff-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

// Import handler functions directly
import { 
  handleOpenDiff, 
  handleOpenRevisionDiff, 
  handleOpenCurrentChanges, 
  handleOpenDiffContent
} from '../../src/tools/diff-tools';

// Mock WebSocket
class MockWebSocket {
  send = jest.fn();
  close = jest.fn();
  readyState = 1; // OPEN
  on = jest.fn();
  
  triggerMessage(data: any) {
    const messageHandler = this.on.mock.calls.find(call => call[0] === 'message')?.[1];
    if (messageHandler) {
      messageHandler(JSON.stringify(data));
    }
  }
}

describe('diff-tools', () => {
  let mockWs: MockWebSocket;
  let mockSend: jest.Mock;
  let responsePromise: Promise<any>;
  let resolveResponse: (value: any) => void;
  let mockBridge: EmacsBridge;
  
  beforeEach(() => {
    jest.clearAllMocks();
    
    mockWs = new MockWebSocket();
    mockSend = jest.fn().mockImplementation((method: string, params: any) => {
      responsePromise = new Promise(resolve => {
        resolveResponse = resolve;
      });
      
      // Simulate sending to Emacs
      mockWs.send({
        jsonrpc: '2.0',
        id: 1,
        method,
        params
      });
      
      return responsePromise;
    });
    
    // Mock the request method of EmacsBridge
    jest.spyOn(EmacsBridge.prototype, 'request').mockImplementation(mockSend);
    
    // Mock the isConnected method of EmacsBridge to return true by default
    jest.spyOn(EmacsBridge.prototype, 'isConnected').mockReturnValue(true);
    
    // Create a mock bridge instance
    mockBridge = new EmacsBridge(jest.fn());
  });

  describe('openDiff', () => {
    it('should compare two files', async () => {
      const params = {
        fileA: 'src/old.js',
        fileB: 'src/new.js'
      };
      
      const resultPromise = handleOpenDiff(mockBridge, params);
      
      // Verify the message sent to Emacs
      expect(mockSend).toHaveBeenCalledWith('openDiff', {
        fileA: 'src/old.js',
        fileB: 'src/new.js'
      });
      
      // Simulate Emacs response
      resolveResponse({ status: 'success', message: 'Opened ediff session' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Opened diff comparison for files: src/old.js and src/new.js' }],
        structuredContent: { status: 'success', message: 'Opened ediff session' }
      });
    });


    it('should handle errors', async () => {
      const params = {
        fileA: 'nonexistent.js',
        fileB: 'alsonothere.js'
      };
      
      const resultPromise = handleOpenDiff(mockBridge, params);
      
      resolveResponse({ status: 'error', message: 'File not found' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Error: File not found' }],
        structuredContent: { status: 'error', message: 'File not found' },
        isError: true
      });
    });

    it('should return error when Emacs is not connected', async () => {
      jest.spyOn(EmacsBridge.prototype, 'isConnected').mockReturnValue(false);
      
      const params = {
        fileA: 'src/old.js',
        fileB: 'src/new.js'
      };
      
      const result = await handleOpenDiff(mockBridge, params);
      
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Error: Emacs is not connected' }],
        structuredContent: { status: 'error', message: 'Emacs is not connected' },
        isError: true
      });
    });

  });

  describe('openRevisionDiff', () => {
    it('should compare file with HEAD by default', async () => {
      const params = {
        file: 'src/index.js'
      };
      
      const resultPromise = handleOpenRevisionDiff(mockBridge, params as any);
      
      expect(mockSend).toHaveBeenCalledWith('openRevisionDiff', {
        file: 'src/index.js',
        revision: 'HEAD'
      });
      
      resolveResponse({ status: 'success', message: 'Opened revision diff' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Comparing src/index.js with revision HEAD' }],
        structuredContent: { status: 'success', message: 'Opened revision diff' }
      });
    });

    it('should compare file with specified revision', async () => {
      const params = {
        file: 'src/index.js',
        revision: 'HEAD~3'
      };
      
      const resultPromise = handleOpenRevisionDiff(mockBridge, params);
      
      expect(mockSend).toHaveBeenCalledWith('openRevisionDiff', params);
      
      resolveResponse({ status: 'success', message: 'Opened revision diff' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Comparing src/index.js with revision HEAD~3' }],
        structuredContent: { status: 'success', message: 'Opened revision diff' }
      });
    });
  });

  describe('openCurrentChanges', () => {
    it('should show unstaged changes for current file', async () => {
      const params = {};
      
      const resultPromise = handleOpenCurrentChanges(mockBridge, params);
      
      expect(mockSend).toHaveBeenCalledWith('openCurrentChanges', {});
      
      resolveResponse({ 
        status: 'success', 
        message: 'Showing changes',
        file: 'current-file.js'
      });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Showing uncommitted changes for current-file.js' }],
        structuredContent: { status: 'success', message: 'Showing changes', file: 'current-file.js' }
      });
    });

    it('should show changes for specified file', async () => {
      const params = {
        file: 'src/modified.js'
      };
      
      const resultPromise = handleOpenCurrentChanges(mockBridge, params);
      
      expect(mockSend).toHaveBeenCalledWith('openCurrentChanges', params);
      
      resolveResponse({ 
        status: 'success',
        message: 'Showing changes',
        file: 'src/modified.js'
      });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Showing uncommitted changes for src/modified.js' }],
        structuredContent: { status: 'success', message: 'Showing changes', file: 'src/modified.js' }
      });
    });
  });

  describe('openDiffContent', () => {
    it('should compare two text contents', async () => {
      const params = {
        contentA: 'Hello World\nLine 2',
        contentB: 'Hello World!\nLine 2 modified',
        titleA: 'Original Content',
        titleB: 'Modified Content'
      };
      
      const resultPromise = handleOpenDiffContent(mockBridge, params);
      
      expect(mockSend).toHaveBeenCalledWith('openDiffContent', {
        contentA: 'Hello World\nLine 2',
        contentB: 'Hello World!\nLine 2 modified',
        titleA: 'Original Content',
        titleB: 'Modified Content'
      });
      
      resolveResponse({ status: 'success', message: 'Opened ediff session' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Opened diff comparison for: Original Content and Modified Content' }],
        structuredContent: { status: 'success', message: 'Opened ediff session' }
      });
    });

    it('should handle errors', async () => {
      const params = {
        contentA: 'content',
        contentB: 'content',
        titleA: 'Title A',
        titleB: 'Title B'
      };
      
      const resultPromise = handleOpenDiffContent(mockBridge, params);
      
      // Wait for the mock to be called
      await new Promise(resolve => setImmediate(resolve));
      
      resolveResponse({ status: 'error', message: 'Failed to create buffers' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Error: Failed to create buffers' }],
        structuredContent: { status: 'error', message: 'Failed to create buffers' },
        isError: true
      });
    });

  });
});
