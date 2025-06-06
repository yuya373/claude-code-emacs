import { describe, it, expect, jest, beforeEach } from '@jest/globals';
import { handleRunCommand } from '../../src/tools/command-tools.js';
import { EmacsBridge } from '../../src/emacs-bridge.js';

describe('handleRunCommand', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn().mockReturnValue(true),
      request: jest.fn()
    } as any;
  });

  it('should execute a simple command successfully', async () => {
    mockBridge.request.mockResolvedValue({
      success: true,
      result: ':null',
      output: '',
      bufferChanged: false
    });

    const result = await handleRunCommand(mockBridge, { command: 'save-buffer' });

    expect(mockBridge.request).toHaveBeenCalledWith('runCommand', { command: 'save-buffer' });
    expect(result.content[0].text).toContain('Executed command: save-buffer');
  });

  it('should pass arguments to command', async () => {
    mockBridge.request.mockResolvedValue({
      success: true,
      result: 100,
      output: '',
      bufferChanged: false
    });

    const result = await handleRunCommand(mockBridge, {
      command: 'goto-line',
      args: [100]
    });

    expect(mockBridge.request).toHaveBeenCalledWith('runCommand', {
      command: 'goto-line',
      args: [100]
    });
    expect(result.content[0].text).toContain('Result: 100');
  });

  it('should handle interactive commands', async () => {
    mockBridge.request.mockResolvedValue({
      success: true,
      result: ':null',
      output: 'Query replace from: foo\nQuery replace to: bar',
      bufferChanged: true
    });

    const result = await handleRunCommand(mockBridge, {
      command: 'query-replace',
      interactive: true
    });

    expect(mockBridge.request).toHaveBeenCalledWith('runCommand', {
      command: 'query-replace',
      interactive: true
    });
    expect(result.content[0].text).toContain('Output: Query replace');
    expect(result.content[0].text).toContain('[Buffer modified]');
  });

  it('should reject blocked commands', async () => {
    await expect(
      handleRunCommand(mockBridge, { command: 'shell-command' })
    ).rejects.toThrow("Command 'shell-command' is not allowed for security reasons");

    await expect(
      handleRunCommand(mockBridge, { command: 'eval-expression' })
    ).rejects.toThrow("Command 'eval-expression' is not allowed for security reasons");
  });

  it('should handle command errors', async () => {
    mockBridge.request.mockResolvedValue({
      success: false,
      error: 'Unknown command: foo-bar-baz'
    });

    const result = await handleRunCommand(mockBridge, { command: 'foo-bar-baz' });

    expect(result.content[0].text).toContain('Failed to execute command: Unknown command: foo-bar-baz');
  });

  it('should throw error when Emacs is not connected', async () => {
    mockBridge.isConnected.mockReturnValue(false);

    await expect(
      handleRunCommand(mockBridge, { command: 'save-buffer' })
    ).rejects.toThrow('Emacs is not connected');
  });

  it('should throw error when command is missing', async () => {
    await expect(
      handleRunCommand(mockBridge, {} as any)
    ).rejects.toThrow('Command is required');
  });

  it('should handle currentBuffer option', async () => {
    mockBridge.request.mockResolvedValue({
      success: true,
      result: ':null',
      output: '',
      bufferChanged: false
    });

    await handleRunCommand(mockBridge, {
      command: 'indent-region',
      currentBuffer: false
    });

    expect(mockBridge.request).toHaveBeenCalledWith('runCommand', {
      command: 'indent-region',
      currentBuffer: false
    });
  });
});