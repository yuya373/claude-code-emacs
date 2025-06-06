import { EmacsBridge } from '../emacs-bridge.js';

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
  if (!bridge.isConnected()) {
    throw new Error('Emacs is not connected');
  }

  if (!args.command) {
    throw new Error('Command is required');
  }

  // Validate command against blocklist
  if (isCommandBlocked(args.command)) {
    throw new Error(`Command '${args.command}' is not allowed for security reasons`);
  }

  const result = await bridge.request('runCommand', args) as CommandResult;

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