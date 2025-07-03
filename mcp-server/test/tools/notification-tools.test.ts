import { handleSendNotification } from '../../src/tools/notification-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

describe('notification-tools', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn().mockReturnValue(true),
      request: jest.fn().mockResolvedValue({ success: true, message: 'Notification sent' })
    } as any;
  });

  describe('handleSendNotification', () => {
    it('should send notification with title and message', async () => {
      const args = {
        title: 'Test Title',
        message: 'Test message content'
      };

      const result = await handleSendNotification(mockBridge, args);

      expect(mockBridge.request).toHaveBeenCalledWith('sendNotification', {
        title: 'Test Title',
        message: 'Test message content'
      });

      expect(result).toEqual({
        content: [
          {
            type: 'text',
            text: 'Notification sent: "Test Title"'
          }
        ],
        status: 'success',
        message: 'Notification sent'
      });
    });

    it('should return error when Emacs is not connected', async () => {
      mockBridge.isConnected.mockReturnValue(false);

      const args = {
        title: 'Test Title',
        message: 'Test message'
      };

      const result = await handleSendNotification(mockBridge, args);
      
      expect(result).toEqual({
        content: [{
          type: 'text',
          text: 'Error: Emacs is not connected'
        }],
        status: 'error',
        message: 'Emacs is not connected',
        isError: true
      });
    });

  });
});