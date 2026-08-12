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
    await bridge.stop();
    jest.useRealTimers();
  });

  // --- shared helpers -------------------------------------------------

  function makeMockWs() {
    return {
      on: jest.fn(),
      send: jest.fn(),
      close: jest.fn(),
      ping: jest.fn(),
      terminate: jest.fn()
    };
  }

  function makeMockReq(sessionId: string = testSessionId) {
    return {
      url: `/?session=${encodeURIComponent(sessionId)}`,
      headers: { host: 'localhost:9999' }
    };
  }

  function mockWebSocketServer() {
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
  }

  async function startBridge(port = 9999): Promise<number> {
    mockWebSocketServer();
    return bridge.start(port, testSessionId);
  }

  /** Start the bridge while jest fake timers are installed. */
  async function startBridgeWithFakeTimers(port = 9999): Promise<number> {
    mockWebSocketServer();
    const promise = bridge.start(port, testSessionId);
    await jest.advanceTimersByTimeAsync(0);
    return promise;
  }

  function connectClient(ws = makeMockWs(), req = makeMockReq()) {
    const connectionCallback = mockWss.on.mock.calls.find((call: any) => call[0] === 'connection');
    expect(connectionCallback).toBeDefined();
    connectionCallback[1](ws, req);
    return ws;
  }

  function wsHandler(ws: any, event: string) {
    const call = ws.on.mock.calls.find((call: any) => call[0] === event);
    expect(call).toBeDefined();
    return call[1];
  }

  // ---------------------------------------------------------------------

  describe('start', () => {
    it('should create WebSocket server on specified port with session ID', async () => {
      const assignedPort = await startBridge(9999);

      expect(assignedPort).toBe(9999);
      expect(WebSocketServer).toHaveBeenCalledWith({
        port: 9999,
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
      await startBridge();
      const mockWs = connectClient();

      expect(mockWs.on).toHaveBeenCalledWith('message', expect.any(Function));
      expect(mockWs.on).toHaveBeenCalledWith('close', expect.any(Function));
      expect(mockWs.on).toHaveBeenCalledWith('error', expect.any(Function));
    });
  });

  describe('connect/disconnect events', () => {
    it('emits connect on connection and disconnect with the close code when the socket closes', async () => {
      await startBridge();

      const connectSpy = jest.fn();
      const disconnectSpy = jest.fn();
      bridge.on('connect', connectSpy);
      bridge.on('disconnect', disconnectSpy);

      const mockWs = connectClient();
      expect(connectSpy).toHaveBeenCalledWith(testSessionId);
      expect(disconnectSpy).not.toHaveBeenCalled();

      // Simulate the socket dropping abnormally (e.g. Emacs restarted)
      wsHandler(mockWs, 'close')(1006);
      expect(disconnectSpy).toHaveBeenCalledWith(testSessionId, 1006);
      expect(bridge.isConnected()).toBe(false);
    });

    it('ignores a stale socket closing after the session has reconnected', async () => {
      await startBridge();

      const disconnectSpy = jest.fn();
      bridge.on('disconnect', disconnectSpy);

      // Old connection drops uncleanly: its close event has not fired
      // yet when Emacs reconnects with a fresh socket
      const oldWs = connectClient();
      const newWs = connectClient(makeMockWs());

      // The replaced socket is terminated so it cannot linger half-open
      expect(oldWs.terminate).toHaveBeenCalled();

      // The old socket's close event must not tear down the new
      // connection nor emit a spurious disconnect
      wsHandler(oldWs, 'close')(1006);
      expect(disconnectSpy).not.toHaveBeenCalled();
      expect(bridge.isConnected()).toBe(true);

      // Closing the current socket still disconnects normally
      wsHandler(newWs, 'close')(1006);
      expect(disconnectSpy).toHaveBeenCalledWith(testSessionId, 1006);
      expect(bridge.isConnected()).toBe(false);
    });
  });

  describe('heartbeat', () => {
    it('terminates a dead socket that misses a pong and keeps a live one', async () => {
      jest.useFakeTimers();
      bridge = new EmacsBridge(jest.fn(), { heartbeatIntervalMs: 30000 });
      await startBridgeWithFakeTimers();

      const mockWs = connectClient();
      expect(mockWs.on).toHaveBeenCalledWith('pong', expect.any(Function));

      // First interval: socket is marked pending and pinged
      await jest.advanceTimersByTimeAsync(30000);
      expect(mockWs.ping).toHaveBeenCalledTimes(1);
      expect(mockWs.terminate).not.toHaveBeenCalled();

      // The client answers the ping: it must survive the next interval
      wsHandler(mockWs, 'pong')();
      await jest.advanceTimersByTimeAsync(30000);
      expect(mockWs.ping).toHaveBeenCalledTimes(2);
      expect(mockWs.terminate).not.toHaveBeenCalled();

      // No pong this time: the socket is dead and gets terminated
      await jest.advanceTimersByTimeAsync(30000);
      expect(mockWs.terminate).toHaveBeenCalled();
    });
  });

  describe('ping/pong', () => {
    it('should respond to ping with pong', async () => {
      await startBridge();
      const mockWs = connectClient();

      // Send ping message
      const pingMessage = JSON.stringify({ type: 'ping' });
      wsHandler(mockWs, 'message')(Buffer.from(pingMessage));

      // Verify pong was sent
      expect(mockWs.send).toHaveBeenCalledWith(JSON.stringify({ type: 'pong' }));
    });

    it('should handle request messages alongside ping messages', async () => {
      await startBridge();
      const mockWs = connectClient();

      // Send request message (not ping)
      const requestMessage = JSON.stringify({
        id: '123',
        method: 'openFile',
        params: { path: 'test.js' }
      });
      wsHandler(mockWs, 'message')(Buffer.from(requestMessage));

      // Should not send pong for non-ping messages
      expect(mockWs.send).not.toHaveBeenCalledWith(JSON.stringify({ type: 'pong' }));
    });
  });

  describe('notifications', () => {
    it('should emit notification events when received', async () => {
      await startBridge();

      // Set up notification handler
      const notificationHandler = jest.fn();
      bridge.setNotificationHandler(notificationHandler);

      const mockWs = connectClient();

      // Send notification message
      const notificationMessage = JSON.stringify({
        method: 'emacs/bufferListUpdated',
        params: {
          buffers: ['/test/file1.el', '/test/file2.el']
        }
      });
      wsHandler(mockWs, 'message')(Buffer.from(notificationMessage));

      // Verify notification handler was called
      expect(notificationHandler).toHaveBeenCalledWith('emacs/bufferListUpdated', {
        buffers: ['/test/file1.el', '/test/file2.el']
      });
    });

    it('should handle different notification types', async () => {
      await startBridge();

      // Set up notification handler
      const notificationHandler = jest.fn();
      bridge.setNotificationHandler(notificationHandler);

      const mockWs = connectClient();
      const messageHandler = wsHandler(mockWs, 'message');

      // Test different notification types
      // 1. Buffer content modified
      const contentModifiedMessage = JSON.stringify({
        method: 'emacs/bufferContentModified',
        params: {
          changes: [{
            file: '/test/file.el',
            startLine: 1,
            endLine: 5,
            changeLength: 100
          }]
        }
      });
      messageHandler(Buffer.from(contentModifiedMessage));

      // 2. Diagnostics changed
      const diagnosticsChangedMessage = JSON.stringify({
        method: 'emacs/diagnosticsChanged',
        params: {
          files: [{
            file: '/test/file.el',
            diagnostics: [{
              line: 10,
              column: 5,
              severity: 'error',
              message: 'Test error'
            }]
          }]
        }
      });
      messageHandler(Buffer.from(diagnosticsChangedMessage));

      // Verify all notifications were handled
      expect(notificationHandler).toHaveBeenCalledTimes(2);
      expect(notificationHandler).toHaveBeenCalledWith('emacs/bufferContentModified', expect.any(Object));
      expect(notificationHandler).toHaveBeenCalledWith('emacs/diagnosticsChanged', expect.any(Object));
    });

    it('should not emit events for non-notification messages', async () => {
      await startBridge();

      // Set up notification handler
      const notificationHandler = jest.fn();
      bridge.setNotificationHandler(notificationHandler);

      const mockWs = connectClient();
      const messageHandler = wsHandler(mockWs, 'message');

      // Send non-notification messages
      // 1. Ping message
      messageHandler(Buffer.from(JSON.stringify({ type: 'ping' })));

      // 2. Response message
      messageHandler(Buffer.from(JSON.stringify({
        id: '123',
        result: 'success'
      })));

      // 3. Request message (has id)
      messageHandler(Buffer.from(JSON.stringify({
        id: '456',
        method: 'someMethod',
        params: {}
      })));

      // Verify notification handler was NOT called
      expect(notificationHandler).not.toHaveBeenCalled();
    });
  });
});
