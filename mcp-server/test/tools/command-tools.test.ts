import { describe, it, expect, jest, beforeEach, afterEach } from '@jest/globals';
import { handleRunCommand } from '../../src/tools/command-tools.js';
import { EmacsBridge } from '../../src/emacs-bridge.js';
import { spawn } from 'child_process';
import { EventEmitter } from 'events';

// Mock child_process
jest.mock('child_process');

describe('handleRunCommand', () => {
  let mockBridge: jest.Mocked<EmacsBridge>;
  let mockSpawn: jest.MockedFunction<typeof spawn>;
  let mockProcess: any;

  beforeEach(() => {
    mockBridge = {
      isConnected: jest.fn().mockReturnValue(true),
      request: jest.fn()
    } as any;

    // Create mock process
    mockProcess = new EventEmitter();
    mockProcess.stdout = new EventEmitter();
    mockProcess.stderr = new EventEmitter();
    
    // Setup spawn mock
    mockSpawn = spawn as jest.MockedFunction<typeof spawn>;
    mockSpawn.mockReturnValue(mockProcess as any);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  it('should execute a simple command successfully', async () => {
    const resultPromise = handleRunCommand(mockBridge, { command: 'save-buffer' });

    // Simulate successful emacsclient execution
    process.nextTick(() => {
      mockProcess.stdout.emit('data', 'nil\n');
      mockProcess.emit('close', 0);
    });

    const result = await resultPromise;

    expect(mockSpawn).toHaveBeenCalledWith('emacsclient', ['-e', '(save-buffer)']);
    expect(result.content[0].text).toContain('Executed command: save-buffer');
  });

  it('should pass arguments to command', async () => {
    const resultPromise = handleRunCommand(mockBridge, {
      command: 'goto-line',
      args: [100]
    });

    process.nextTick(() => {
      mockProcess.stdout.emit('data', '100\n');
      mockProcess.emit('close', 0);
    });

    const result = await resultPromise;

    expect(mockSpawn).toHaveBeenCalledWith('emacsclient', ['-e', '(goto-line 100)']);
    expect(result.content[0].text).toContain('Result: "100"');
  });

  it('should handle interactive commands', async () => {
    const resultPromise = handleRunCommand(mockBridge, {
      command: 'query-replace',
      interactive: true
    });

    process.nextTick(() => {
      mockProcess.stdout.emit('data', 'nil\n');
      mockProcess.emit('close', 0);
    });

    const result = await resultPromise;

    expect(mockSpawn).toHaveBeenCalledWith('emacsclient', ['-e', "(call-interactively 'query-replace)"]);
    expect(result.content[0].text).toContain('Executed command: query-replace');
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
    const resultPromise = handleRunCommand(mockBridge, { command: 'foo-bar-baz' });

    process.nextTick(() => {
      mockProcess.stderr.emit('data', 'Unknown command: foo-bar-baz');
      mockProcess.emit('close', 1);
    });

    const result = await resultPromise;

    expect(result.content[0].text).toContain('Failed to execute command: Unknown command: foo-bar-baz');
  });

  it('should handle emacsclient spawn errors', async () => {
    const resultPromise = handleRunCommand(mockBridge, { command: 'save-buffer' });

    process.nextTick(() => {
      mockProcess.emit('error', new Error('spawn emacsclient ENOENT'));
    });

    const result = await resultPromise;
    
    expect(result.content[0].text).toContain('Failed to execute command: Failed to run emacsclient');
  });

  it('should throw error when command is missing', async () => {
    await expect(
      handleRunCommand(mockBridge, {} as any)
    ).rejects.toThrow('Command is required');
  });

  it('should handle string arguments with quotes', async () => {
    const resultPromise = handleRunCommand(mockBridge, {
      command: 'insert',
      args: ['Hello "World"']
    });

    process.nextTick(() => {
      mockProcess.stdout.emit('data', 'nil\n');
      mockProcess.emit('close', 0);
    });

    await resultPromise;

    expect(mockSpawn).toHaveBeenCalledWith('emacsclient', ['-e', '(insert "Hello \\"World\\"")']);
  });

  it('should handle boolean and number arguments', async () => {
    const resultPromise = handleRunCommand(mockBridge, {
      command: 'set-variable',
      args: ['truncate-lines', true, 42]
    });

    process.nextTick(() => {
      mockProcess.stdout.emit('data', 't\n');
      mockProcess.emit('close', 0);
    });

    await resultPromise;

    expect(mockSpawn).toHaveBeenCalledWith('emacsclient', ['-e', '(set-variable "truncate-lines" t 42)']);
  });
});