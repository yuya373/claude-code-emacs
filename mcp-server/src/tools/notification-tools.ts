import { EmacsBridge } from '../emacs-bridge.js';
import { NotificationArgs } from '../schemas/notification-schema.js';

interface NotificationToolResult {
  content: Array<{ type: 'text'; text: string }>;
  status: 'success' | 'error';
  message?: string;
  isError?: boolean;
}

export async function handleSendNotification(bridge: EmacsBridge, args: NotificationArgs): Promise<NotificationToolResult> {
  if (!bridge.isConnected()) {
    return {
      content: [
        {
          type: 'text' as const,
          text: 'Error: Emacs is not connected'
        }
      ],
      status: 'error',
      message: 'Emacs is not connected',
      isError: true
    };
  }

  try {
    // Send notification to Emacs
    const result = await bridge.request('sendNotification', {
      title: args.title,
      message: args.message
    });

    // Emacs returns { success: true, message: "Notification sent" }
    // We need to convert it to { status: 'success', message: "..." }
    const success = (result as any).success === true;

    if (!success) {
      return {
        content: [
          {
            type: 'text' as const,
            text: 'Failed to send notification'
          }
        ],
        status: 'error',
        message: 'Failed to send notification',
        isError: true
      };
    }

    return {
      content: [
        {
          type: 'text' as const,
          text: `Notification sent: "${args.title}"`
        }
      ],
      status: 'success',
      message: 'Notification sent'
    };
  } catch (error) {
    return {
      content: [
        {
          type: 'text' as const,
          text: `Error sending notification: ${error instanceof Error ? error.message : 'Unknown error'}`
        }
      ],
      status: 'error',
      message: error instanceof Error ? error.message : 'Unknown error',
      isError: true
    };
  }
}
