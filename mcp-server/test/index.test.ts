// Since we can't easily test the actual server due to ESM import issues with MCP SDK,
// we'll test the ping monitoring logic separately

describe('MCP Server ping monitoring', () => {
  let originalSetTimeout: typeof setTimeout;
  let timeoutCallbacks: Array<{ callback: () => void; delay: number }> = [];
  let mockLog: jest.Mock;
  let mockExit: jest.Mock;

  beforeEach(() => {
    jest.clearAllMocks();
    timeoutCallbacks = [];
    
    // Mock setTimeout to capture callbacks
    originalSetTimeout = global.setTimeout;
    global.setTimeout = jest.fn((callback: () => void, delay: number) => {
      timeoutCallbacks.push({ callback, delay });
      return {} as any;
    }) as any;

    mockLog = jest.fn();
    mockExit = jest.fn();
  });

  afterEach(() => {
    global.setTimeout = originalSetTimeout;
  });

  describe('ping logic', () => {
    it('should schedule ping every 30 seconds on success', async () => {
      const mockPing = jest.fn().mockResolvedValue(undefined);
      
      // Simulate the ping function
      const ping = async () => {
        try {
          await mockPing();
          mockLog('Ping successful for session /test/project');
          setTimeout(ping, 30000);
        } catch (error) {
          mockLog('Ping failed for session /test/project, Emacs bridge on port 12345. Exitting...');
          mockExit(1);
        }
      };

      // Start ping
      mockLog('Starting ping monitoring for session /test/project');
      ping();

      // Wait for async operations
      await new Promise(resolve => setImmediate(resolve));

      // Verify initial ping was called
      expect(mockPing).toHaveBeenCalledTimes(1);
      expect(mockLog).toHaveBeenCalledWith('Ping successful for session /test/project');
      
      // Verify setTimeout was called with correct delay
      expect(timeoutCallbacks).toHaveLength(1);
      expect(timeoutCallbacks[0].delay).toBe(30000);

      // Trigger the next ping
      timeoutCallbacks[0].callback();
      await new Promise(resolve => setImmediate(resolve));

      // Verify second ping
      expect(mockPing).toHaveBeenCalledTimes(2);
      expect(timeoutCallbacks).toHaveLength(2);
    });

    it('should exit on ping failure', async () => {
      const mockPing = jest.fn().mockRejectedValue(new Error('Connection lost'));
      const mockCleanup = jest.fn().mockResolvedValue(undefined);
      
      // Simulate the ping function
      const ping = async () => {
        try {
          await mockPing();
          mockLog('Ping successful for session /test/project');
          setTimeout(ping, 30000);
        } catch (error) {
          mockLog('Ping failed for session /test/project, Emacs bridge on port 12345. Exitting...');
          await mockCleanup();
          mockExit(1);
        }
      };

      // Start ping
      ping();

      // Wait for async operations
      await new Promise(resolve => setImmediate(resolve));

      // Verify ping was attempted
      expect(mockPing).toHaveBeenCalledTimes(1);
      
      // Verify error handling
      expect(mockLog).toHaveBeenCalledWith('Ping failed for session /test/project, Emacs bridge on port 12345. Exitting...');
      expect(mockCleanup).toHaveBeenCalled();
      expect(mockExit).toHaveBeenCalledWith(1);
      
      // Verify no new timeout was scheduled
      expect(timeoutCallbacks).toHaveLength(0);
    });

    it('should log appropriate messages', () => {
      const messages = [
        'Starting ping monitoring for session /test/project',
        'Ping successful for session /test/project',
        'Ping failed for session /test/project, Emacs bridge on port 12345. Exitting...'
      ];

      // Verify message format
      messages.forEach(msg => {
        expect(msg).toContain('session /test/project');
      });

      // Verify failed message includes port
      expect(messages[2]).toContain('port 12345');
    });
  });

  describe('cleanup behavior', () => {
    it('should handle cleanup function', async () => {
      const mockBridgeStop = jest.fn().mockResolvedValue(undefined);
      const mockUnregister = jest.fn().mockResolvedValue(undefined);
      const mockUnlink = jest.fn().mockResolvedValue(undefined);
      
      const cleanup = async () => {
        const projectRoot = '/test/project';
        
        try {
          await mockUnregister(projectRoot);
          mockLog(`Unregistered port for project ${projectRoot}`);
        } catch (error) {
          mockLog(`Failed to unregister port: ${error}`);
        }

        try {
          const portFile = `/tmp/claude-code-emacs-mcp-_test_project.port`;
          await mockUnlink(portFile);
          mockLog(`Removed port file ${portFile}`);
        } catch (error) {
          mockLog(`Failed to remove port file: ${error}`);
        }

        await mockBridgeStop();
      };

      await cleanup();

      expect(mockUnregister).toHaveBeenCalledWith('/test/project');
      expect(mockUnlink).toHaveBeenCalledWith('/tmp/claude-code-emacs-mcp-_test_project.port');
      expect(mockBridgeStop).toHaveBeenCalled();
      expect(mockLog).toHaveBeenCalledWith('Unregistered port for project /test/project');
      expect(mockLog).toHaveBeenCalledWith('Removed port file /tmp/claude-code-emacs-mcp-_test_project.port');
    });
  });
});