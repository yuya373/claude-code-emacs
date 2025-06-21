import { handleSendNotification } from '../../src/tools/notification-tools';
import { EmacsBridge } from '../../src/emacs-bridge';

describe('notification-tools', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn().mockReturnValue(true),
      request: jest.fn().mockResolvedValue(undefined)
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
        ]
      });
    });

    it('should throw error when Emacs is not connected', async () => {
      mockBridge.isConnected.mockReturnValue(false);

      const args = {
        title: 'Test Title',
        message: 'Test message'
      };

      await expect(handleSendNotification(mockBridge, args))
        .rejects.toThrow('Emacs is not connected');
    });

    it('should throw error when title is missing', async () => {
      const args = {
        message: 'Test message'
      } as any;

      await expect(handleSendNotification(mockBridge, args))
        .rejects.toThrow('Title is required');
    });

    it('should throw error when message is missing', async () => {
      const args = {
        title: 'Test Title'
      } as any;

      await expect(handleSendNotification(mockBridge, args))
        .rejects.toThrow('Message is required');
    });

  });
});