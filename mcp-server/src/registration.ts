// Helpers for registering this MCP server instance with Emacs.
//
// Each MCP server process generates a unique instance ID so that
// multiple Claude Code sessions in the same project can hold
// independent connections to Emacs. Registration passes the instance
// ID to Emacs; unregistration tears down only this instance's
// connection, never another agent's.

export function buildRegisterElisp(
  projectRoot: string,
  port: number,
  instanceId: string
): string {
  return `(claude-code-mcp-register-port "${projectRoot}" ${port} "${instanceId}")`;
}

export function buildUnregisterElisp(instanceId: string): string {
  return `(claude-code-mcp-unregister-port "${instanceId}")`;
}

export function portFileName(projectRoot: string, instanceId: string): string {
  const sanitizedRoot = projectRoot.replace(/[^a-zA-Z0-9]/g, '_');
  return `claude-code-mcp-${sanitizedRoot}-${instanceId}.port`;
}
