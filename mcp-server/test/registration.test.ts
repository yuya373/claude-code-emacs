import {
  buildRegisterElisp,
  buildUnregisterElisp,
  portFileName
} from '../src/registration';

describe('registration', () => {
  describe('buildRegisterElisp', () => {
    it('includes project root, port and instance id', () => {
      expect(buildRegisterElisp('/test/project', 12345, 'abc-123')).toBe(
        '(claude-code-mcp-register-port "/test/project" 12345 "abc-123")'
      );
    });
  });

  describe('buildUnregisterElisp', () => {
    it('unregisters by instance id only', () => {
      expect(buildUnregisterElisp('abc-123')).toBe(
        '(claude-code-mcp-unregister-port "abc-123")'
      );
    });
  });

  describe('portFileName', () => {
    it('is unique per instance so parallel agents do not clobber each other', () => {
      const a = portFileName('/test/project', 'abc-123');
      const b = portFileName('/test/project', 'def-456');
      expect(a).not.toBe(b);
      expect(a).toContain('claude-code-mcp-');
      expect(a).toContain('abc-123');
    });

    it('sanitizes the project root', () => {
      expect(portFileName('/test/project', 'abc-123')).not.toContain('/test/project');
    });
  });
});
