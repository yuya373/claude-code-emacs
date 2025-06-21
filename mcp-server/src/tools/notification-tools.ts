import { EmacsBridge } from '../emacs-bridge.js';

export interface NotificationArgs {
  title: string;
  message: string;
}

export async function handleSendNotification(bridge: EmacsBridge, args: NotificationArgs) {
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  // Validate required parameters
  if (!args.title) {
    throw new Error('Title is required');
  }
  if (!args.message) {
    throw new Error('Message is required');
  }

  // Send notification to Emacs
  await bridge.request('sendNotification', {
    title: args.title,
    message: args.message
  });

  return {
    content: [
      {
        type: 'text',
        text: `Notification sent: "${args.title}"`
      }
    ]
  };
}