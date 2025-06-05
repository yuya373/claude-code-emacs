import { diffTools } from '../../src/tools/diff-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

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
  });

  describe('openDiff', () => {
    it('should compare two files', async () => {
      const handler = diffTools.openDiff.handler;
      const params = {
        fileA: 'src/old.js',
        fileB: 'src/new.js'
      };
      
      const resultPromise = handler(params);
      
      // Verify the message sent to Emacs
      expect(mockSend).toHaveBeenCalledWith('openDiff', {
        fileA: 'src/old.js',
        fileB: 'src/new.js',
        mode: 'files'
      });
      
      // Simulate Emacs response
      resolveResponse({ status: 'success', message: 'Opened ediff session' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Opened ediff session for files: src/old.js and src/new.js' }]
      });
    });

    it('should compare two buffers', async () => {
      const handler = diffTools.openDiff.handler;
      const params = {
        bufferA: '*scratch*',
        bufferB: '*Messages*'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openDiff', {
        bufferA: '*scratch*',
        bufferB: '*Messages*',
        mode: 'buffers'
      });
      
      resolveResponse({ status: 'success', message: 'Opened ediff session' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Opened ediff session for buffers: *scratch* and *Messages*' }]
      });
    });

    it('should handle errors', async () => {
      const handler = diffTools.openDiff.handler;
      const params = {
        fileA: 'nonexistent.js',
        fileB: 'alsonothere.js'
      };
      
      const resultPromise = handler(params);
      
      resolveResponse({ status: 'error', message: 'File not found' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Error: File not found' }]
      });
    });
  });

  describe('openDiff3', () => {
    it('should compare three files', async () => {
      const handler = diffTools.openDiff3.handler;
      const params = {
        fileA: 'mine.js',
        fileB: 'theirs.js',
        fileC: 'base.js'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openDiff3', params);
      
      resolveResponse({ status: 'success', message: 'Opened ediff3 session' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Opened 3-way diff for: mine.js, theirs.js, base.js' }]
      });
    });

    it('should support ancestor for merge', async () => {
      const handler = diffTools.openDiff3.handler;
      const params = {
        fileA: 'mine.js',
        fileB: 'theirs.js',
        fileC: 'result.js',
        ancestor: 'base.js'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openDiff3', params);
      
      resolveResponse({ status: 'success', message: 'Opened merge session' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Opened merge session with ancestor: base.js' }]
      });
    });
  });

  describe('openRevisionDiff', () => {
    it('should compare file with HEAD by default', async () => {
      const handler = diffTools.openRevisionDiff.handler;
      const params = {
        file: 'src/index.js'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openRevisionDiff', {
        file: 'src/index.js',
        revision: 'HEAD'
      });
      
      resolveResponse({ status: 'success', message: 'Opened revision diff' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Comparing src/index.js with revision HEAD' }]
      });
    });

    it('should compare file with specified revision', async () => {
      const handler = diffTools.openRevisionDiff.handler;
      const params = {
        file: 'src/index.js',
        revision: 'HEAD~3'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openRevisionDiff', params);
      
      resolveResponse({ status: 'success', message: 'Opened revision diff' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Comparing src/index.js with revision HEAD~3' }]
      });
    });
  });

  describe('openCurrentChanges', () => {
    it('should show unstaged changes for current file', async () => {
      const handler = diffTools.openCurrentChanges.handler;
      const params = {};
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openCurrentChanges', {});
      
      resolveResponse({ 
        status: 'success', 
        message: 'Showing changes',
        file: 'current-file.js'
      });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Showing uncommitted changes for current-file.js' }]
      });
    });

    it('should show changes for specified file', async () => {
      const handler = diffTools.openCurrentChanges.handler;
      const params = {
        file: 'src/modified.js'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('openCurrentChanges', params);
      
      resolveResponse({ 
        status: 'success',
        message: 'Showing changes',
        file: 'src/modified.js'
      });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Showing uncommitted changes for src/modified.js' }]
      });
    });
  });

  describe('applyPatch', () => {
    it('should apply patch to target file', async () => {
      const handler = diffTools.applyPatch.handler;
      const params = {
        patchFile: 'fix.patch',
        targetFile: 'src/buggy.js'
      };
      
      const resultPromise = handler(params);
      
      expect(mockSend).toHaveBeenCalledWith('applyPatch', params);
      
      resolveResponse({ status: 'success', message: 'Patch session started' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Applying patch fix.patch to src/buggy.js' }]
      });
    });

    it('should handle patch errors', async () => {
      const handler = diffTools.applyPatch.handler;
      const params = {
        patchFile: 'bad.patch',
        targetFile: 'src/file.js'
      };
      
      const resultPromise = handler(params);
      
      resolveResponse({ status: 'error', message: 'Invalid patch format' });
      
      const result = await resultPromise;
      expect(result).toEqual({
        content: [{ type: 'text', text: 'Error: Invalid patch format' }]
      });
    });
  });
});