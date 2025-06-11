import { EmacsBridge } from '../emacs-bridge.js';
import { spawn } from 'child_process';
import { promisify } from 'util';

export interface RunCommandArgs {
  command: string;           // Emacs command to execute (e.g., "save-buffer", "goto-line")
  args?: any[];             // Arguments for the command
  interactive?: boolean;     // Whether the command should be run interactively
  currentBuffer?: boolean;   // Whether to run in current buffer context
}

interface CommandResult {
  success: boolean;
  result?: any;
  error?: string;
  output?: string;          // Any output from the command
  bufferChanged?: boolean;  // Whether the buffer was modified
}

export async function handleRunCommand(bridge: EmacsBridge, args: RunCommandArgs): Promise<any> {
  if (!args.command) {
    throw new Error('Command is required');
  }

  // Validate command against blocklist
  if (isCommandBlocked(args.command)) {
    throw new Error(`Command '${args.command}' is not allowed for security reasons`);
  }

  try {
    // Build emacsclient expression
    let expression: string;
    if (args.interactive) {
      expression = `(call-interactively '${args.command})`;
    } else if (args.args && args.args.length > 0) {
      // Convert args to Lisp format
      const lispArgs = args.args.map(arg => {
        if (typeof arg === 'string') {
          return `"${arg.replace(/"/g, '\\"')}"`;
        } else if (typeof arg === 'number') {
          return arg.toString();
        } else if (typeof arg === 'boolean') {
          return arg ? 't' : 'nil';
        } else {
          return JSON.stringify(arg);
        }
      }).join(' ');
      expression = `(${args.command} ${lispArgs})`;
    } else {
      expression = `(${args.command})`;
    }

    // Execute via emacsclient
    const result = await new Promise<CommandResult>((resolve) => {
      const emacsclient = spawn('emacsclient', ['-e', expression]);

      let stdout = '';
      let stderr = '';

      emacsclient.stdout.on('data', (data) => {
        stdout += data.toString();
      });

      emacsclient.stderr.on('data', (data) => {
        stderr += data.toString();
      });

      emacsclient.on('close', (code) => {
        if (code === 0) {
          resolve({
            success: true,
            output: stdout.trim(),
            result: stdout.trim()
          });
        } else {
          resolve({
            success: false,
            error: stderr || `Command exited with code ${code}`
          });
        }
      });

      emacsclient.on('error', (err) => {
        resolve({
          success: false,
          error: `Failed to run emacsclient: ${err.message}`
        });
      });
    });

    return {
      content: [
        {
          type: 'text',
          text: result.success
            ? formatSuccessMessage(args.command, result)
            : `Failed to execute command: ${result.error}`
        }
      ]
    };
  } catch (error) {
    throw new Error(`Failed to execute command: ${error instanceof Error ? error.message : String(error)}`);
  }
}

// Security: Block potentially dangerous commands
function isCommandBlocked(command: string): boolean {
  const blockedCommands = [
    // System/Shell commands
    'shell-command',
    'async-shell-command',
    'compile',
    'shell',
    'term',
    'eshell',

    // File system destructive operations
    'delete-file',
    'delete-directory',
    'move-file-to-trash',

    // Emacs exit/quit
    'save-buffers-kill-emacs',
    'kill-emacs',

    // Package management (could install malicious code)
    'package-install',
    'package-delete',

    // Evaluation of arbitrary code
    'eval-expression',
    'eval-region',
    'eval-buffer',
    'eval-last-sexp',
    'eval-defun',

    // Loading arbitrary files
    'load-file',
    'load-library',

    // Network operations
    'url-retrieve',
    'browse-url',
  ];

  return blockedCommands.includes(command);
}

function formatSuccessMessage(command: string, result: CommandResult): string {
  let message = `Executed command: ${command}`;

  if (result.output) {
    message += `\nOutput: ${result.output}`;
  }

  if (result.bufferChanged) {
    message += '\n[Buffer modified]';
  }

  if (result.result !== undefined && result.result !== null && result.result !== ':null') {
    message += `\nResult: ${JSON.stringify(result.result)}`;
  }

  return message;
}
