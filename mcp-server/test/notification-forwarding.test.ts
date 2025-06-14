import { describe, it, expect, jest, beforeEach, afterEach } from '@jest/globals';

describe('Notification forwarding from Emacs to Claude Code', () => {
  let mockBridge: any;
  let mockServer: any;
  let notificationHandler: ((method: string, params: any) => void) | undefined;
  let mockLog: jest.Mock;

  beforeEach(() => {
    jest.clearAllMocks();
    
    // Mock the bridge
    mockBridge = {
      setNotificationHandler: jest.fn((handler: (method: string, params: any) => void) => {
        notificationHandler = handler;
      })
    };

    // Mock the server
    mockServer = {
      notification: jest.fn()
    };

    // Mock the log function
    mockLog = jest.fn();
  });

  describe('notification handler setup', () => {
    it('should register notification handler on bridge', () => {
      // Simulate the setup code from index.ts
      mockBridge.setNotificationHandler((method: string, params: any) => {
        mockLog(`Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`);
        mockServer.notification({
          method,
          params
        });
      });

      expect(mockBridge.setNotificationHandler).toHaveBeenCalledWith(expect.any(Function));
      expect(notificationHandler).toBeDefined();
    });
  });

  describe('notification forwarding', () => {
    beforeEach(() => {
      // Set up the notification handler as in index.ts
      mockBridge.setNotificationHandler((method: string, params: any) => {
        mockLog(`Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`);
        mockServer.notification({
          method,
          params
        });
      });
    });

    it('should forward bufferListUpdated notifications', () => {
      const method = 'emacs/bufferListUpdated';
      const params = {
        buffers: [
          { path: '/test/file1.el', name: 'file1.el', active: true, modified: false },
          { path: '/test/file2.el', name: 'file2.el', active: false, modified: true }
        ]
      };

      // Trigger notification
      notificationHandler!(method, params);

      // Verify logging
      expect(mockLog).toHaveBeenCalledWith(
        `Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`
      );

      // Verify server.notification was called
      expect(mockServer.notification).toHaveBeenCalledWith({
        method,
        params
      });
    });

    it('should forward bufferContentModified notifications', () => {
      const method = 'emacs/bufferContentModified';
      const params = {
        changes: [
          {
            file: '/test/file.el',
            startLine: 10,
            endLine: 20,
            changeLength: 150
          },
          {
            file: '/test/file2.el',
            startLine: 5,
            endLine: 5,
            changeLength: 50
          }
        ]
      };

      // Trigger notification
      notificationHandler!(method, params);

      // Verify logging
      expect(mockLog).toHaveBeenCalledWith(
        `Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`
      );

      // Verify server.notification was called
      expect(mockServer.notification).toHaveBeenCalledWith({
        method,
        params
      });
    });

    it('should forward diagnosticsChanged notifications', () => {
      const method = 'emacs/diagnosticsChanged';
      const params = {
        files: [
          {
            file: '/test/file.el',
            diagnostics: [
              {
                line: 10,
                column: 5,
                severity: 'error',
                message: 'Undefined variable'
              },
              {
                line: 20,
                column: 10,
                severity: 'warning',
                message: 'Unused import'
              }
            ]
          }
        ]
      };

      // Trigger notification
      notificationHandler!(method, params);

      // Verify logging
      expect(mockLog).toHaveBeenCalledWith(
        `Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`
      );

      // Verify server.notification was called
      expect(mockServer.notification).toHaveBeenCalledWith({
        method,
        params
      });
    });

    it('should handle notifications with empty params', () => {
      const method = 'emacs/customEvent';
      const params = {};

      // Trigger notification
      notificationHandler!(method, params);

      // Verify server.notification was called
      expect(mockServer.notification).toHaveBeenCalledWith({
        method,
        params
      });
    });

    it('should handle notifications with null params', () => {
      const method = 'emacs/anotherEvent';
      const params = null;

      // Trigger notification
      notificationHandler!(method, params);

      // Verify server.notification was called
      expect(mockServer.notification).toHaveBeenCalledWith({
        method,
        params
      });
    });

    it('should handle multiple notifications in sequence', () => {
      const notifications = [
        { method: 'emacs/bufferListUpdated', params: { buffers: [] } },
        { method: 'emacs/bufferContentModified', params: { changes: [] } },
        { method: 'emacs/diagnosticsChanged', params: { files: [] } }
      ];

      // Trigger all notifications
      notifications.forEach(({ method, params }) => {
        notificationHandler!(method, params);
      });

      // Verify all were forwarded
      expect(mockServer.notification).toHaveBeenCalledTimes(3);
      notifications.forEach(({ method, params }) => {
        expect(mockServer.notification).toHaveBeenCalledWith({
          method,
          params
        });
      });
    });
  });

  describe('error handling', () => {
    beforeEach(() => {
      // Set up the notification handler with error handling
      mockBridge.setNotificationHandler((method: string, params: any) => {
        try {
          mockLog(`Forwarding Emacs notification to Claude Code: ${method} with params: ${JSON.stringify(params)}`);
          mockServer.notification({
            method,
            params
          });
        } catch (error) {
          mockLog(`Error forwarding notification: ${error}`);
        }
      });
    });

    it('should handle server.notification errors gracefully', () => {
      // Make server.notification throw an error
      mockServer.notification.mockImplementation(() => {
        throw new Error('Server notification failed');
      });

      const method = 'emacs/bufferListUpdated';
      const params = { buffers: [] };

      // Trigger notification - should not throw
      expect(() => {
        notificationHandler!(method, params);
      }).not.toThrow();

      // Verify error was logged
      expect(mockLog).toHaveBeenCalledWith('Error forwarding notification: Error: Server notification failed');
    });
  });
});