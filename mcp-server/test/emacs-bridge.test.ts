import { EmacsBridge } from '../src/emacs-bridge';
import { WebSocketServer } from 'ws';

describe('EmacsBridge', () => {
  let bridge: EmacsBridge;
  let mockWss: any;
  const testSessionId = '/test/project';

  beforeEach(() => {
    const mockLogger = jest.fn();
    bridge = new EmacsBridge(mockLogger);
    mockWss = {
      on: jest.fn(),
      close: jest.fn((cb) => cb && cb()),
      address: jest.fn(() => ({ port: 9999 }))
    };
  });

  afterEach(async () => {
    // Mock bridges don't need explicit cleanup
  });

  describe('start', () => {
    it('should create WebSocket server on specified port with session ID', async () => {
      const port = 9999;
      
      // Mock WebSocketServer constructor
      (WebSocketServer as any) = jest.fn().mockImplementation(() => {
        const wss = mockWss;
        // Simulate successful start
        setTimeout(() => {
          const listeningCallback = wss.on.mock.calls.find((call: any) => call[0] === 'listening');
          if (listeningCallback) {
            listeningCallback[1]();
          }
        }, 0);
        return wss;
      });

      const assignedPort = await bridge.start(port, testSessionId);

      expect(assignedPort).toBe(9999);
      expect(WebSocketServer).toHaveBeenCalledWith({ 
        port,
        verifyClient: expect.any(Function)
      });
      expect(mockWss.on).toHaveBeenCalledWith('connection', expect.any(Function));
      expect(mockWss.on).toHaveBeenCalledWith('listening', expect.any(Function));
      expect(mockWss.on).toHaveBeenCalledWith('error', expect.any(Function));
      expect(mockWss.on).toHaveBeenCalledWith('headers', expect.any(Function));
    });
  });

  describe('isConnected', () => {
    it('should return false when no clients connected', () => {
      expect(bridge.isConnected()).toBe(false);
    });
  });

  describe('request', () => {
    it('should throw error when no client connected', async () => {
      await expect(bridge.request('test', {})).rejects.toThrow('No Emacs client connected');
    });
  });

  describe('multiple sessions', () => {
    it('should handle connection callback with session parameter', async () => {
      const port = 9999;
      const mockWs = {
        on: jest.fn(),
        send: jest.fn(),
        close: jest.fn()
      };
      const mockReq = {
        url: `/?session=${encodeURIComponent(testSessionId)}`,
        headers: { host: 'localhost:9999' }
      };
      
      // Mock WebSocketServer constructor
      (WebSocketServer as any) = jest.fn().mockImplementation(() => {
        const wss = mockWss;
        // Simulate successful start
        setTimeout(() => {
          const listeningCallback = wss.on.mock.calls.find((call: any) => call[0] === 'listening');
          if (listeningCallback) {
            listeningCallback[1]();
          }
        }, 0);
        return wss;
      });

      await bridge.start(port, testSessionId);

      // Simulate connection
      const connectionCallback = mockWss.on.mock.calls.find((call: any) => call[0] === 'connection');
      expect(connectionCallback).toBeDefined();
      
      // Call the connection handler
      connectionCallback[1](mockWs, mockReq);
      
      expect(mockWs.on).toHaveBeenCalledWith('message', expect.any(Function));
      expect(mockWs.on).toHaveBeenCalledWith('close', expect.any(Function));
      expect(mockWs.on).toHaveBeenCalledWith('error', expect.any(Function));
    });
  });

  describe('ping/pong', () => {
    it('should respond to ping with pong', async () => {
      const port = 9999;
      const mockWs = {
        on: jest.fn(),
        send: jest.fn(),
        close: jest.fn()
      };
      const mockReq = {
        url: `/?session=${encodeURIComponent(testSessionId)}`,
        headers: { host: 'localhost:9999' }
      };
      
      // Mock WebSocketServer constructor
      (WebSocketServer as any) = jest.fn().mockImplementation(() => {
        const wss = mockWss;
        // Simulate successful start
        setTimeout(() => {
          const listeningCallback = wss.on.mock.calls.find((call: any) => call[0] === 'listening');
          if (listeningCallback) {
            listeningCallback[1]();
          }
        }, 0);
        return wss;
      });

      await bridge.start(port, testSessionId);

      // Simulate connection
      const connectionCallback = mockWss.on.mock.calls.find((call: any) => call[0] === 'connection');
      connectionCallback[1](mockWs, mockReq);
      
      // Get the message handler
      const messageCallback = mockWs.on.mock.calls.find((call: any) => call[0] === 'message');
      expect(messageCallback).toBeDefined();
      
      // Send ping message
      const pingMessage = JSON.stringify({ type: 'ping' });
      messageCallback[1](Buffer.from(pingMessage));
      
      // Verify pong was sent
      expect(mockWs.send).toHaveBeenCalledWith(JSON.stringify({ type: 'pong' }));
    });

    it('should handle request messages alongside ping messages', async () => {
      const port = 9999;
      const mockWs = {
        on: jest.fn(),
        send: jest.fn(),
        close: jest.fn()
      };
      const mockReq = {
        url: `/?session=${encodeURIComponent(testSessionId)}`,
        headers: { host: 'localhost:9999' }
      };
      
      // Mock WebSocketServer constructor
      (WebSocketServer as any) = jest.fn().mockImplementation(() => {
        const wss = mockWss;
        // Simulate successful start
        setTimeout(() => {
          const listeningCallback = wss.on.mock.calls.find((call: any) => call[0] === 'listening');
          if (listeningCallback) {
            listeningCallback[1]();
          }
        }, 0);
        return wss;
      });

      await bridge.start(port, testSessionId);

      // Simulate connection
      const connectionCallback = mockWss.on.mock.calls.find((call: any) => call[0] === 'connection');
      connectionCallback[1](mockWs, mockReq);
      
      // Get the message handler
      const messageCallback = mockWs.on.mock.calls.find((call: any) => call[0] === 'message');
      
      // Send request message (not ping)
      const requestMessage = JSON.stringify({ 
        id: '123', 
        method: 'openFile', 
        params: { path: 'test.js' } 
      });
      messageCallback[1](Buffer.from(requestMessage));
      
      // Should not send pong for non-ping messages
      expect(mockWs.send).not.toHaveBeenCalledWith(JSON.stringify({ type: 'pong' }));
    });
  });
});