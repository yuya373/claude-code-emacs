import { EmacsBridge } from '../src/emacs-bridge';
import { WebSocketServer } from 'ws';

describe('EmacsBridge', () => {
  let bridge: EmacsBridge;
  let mockWss: any;

  beforeEach(() => {
    bridge = new EmacsBridge();
    mockWss = {
      on: jest.fn(),
      close: jest.fn((cb) => cb && cb())
    };
  });

  afterEach(() => {
    if (bridge) {
      bridge.stop();
    }
  });

  describe('start', () => {
    it('should create WebSocket server on specified port', async () => {
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

      await bridge.start(port);

      expect(WebSocketServer).toHaveBeenCalledWith({ port });
      expect(mockWss.on).toHaveBeenCalledWith('connection', expect.any(Function));
      expect(mockWss.on).toHaveBeenCalledWith('listening', expect.any(Function));
      expect(mockWss.on).toHaveBeenCalledWith('error', expect.any(Function));
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
});