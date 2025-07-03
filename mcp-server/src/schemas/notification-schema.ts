import { z } from 'zod';

// Input schema for sendNotification tool
export const sendNotificationInputSchema = z.object({
  title: z.string().describe('Title of the notification'),
  message: z.string().describe('Message content of the notification')
});

// Output schema for sendNotification tool
export const sendNotificationOutputSchema = z.object({
  status: z.enum(['success', 'error']),
  message: z.string().optional()
});

// Inferred types from schemas
export type NotificationArgs = z.infer<typeof sendNotificationInputSchema>;
export type NotificationResult = z.infer<typeof sendNotificationOutputSchema>;
