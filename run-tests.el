;;; run-tests.el --- Test runner for claude-code-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Run this file to execute all tests:
;; emacs -batch -l run-tests.el

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Initialize package system
(require 'package)
(package-initialize)

;; Disable MCP auto-connect for tests before loading anything
(setq claude-code-emacs-mcp-auto-connect nil)

;; Load all modules
(require 'claude-code-emacs)

;; Load test files in order
;; Core module tests
(load-file "test-claude-code-emacs-core.el")
(load-file "test-claude-code-emacs-buffer.el")
(load-file "test-claude-code-emacs-commands.el")
(load-file "test-claude-code-emacs-ui.el")
(load-file "test-claude-code-emacs-prompt.el")

;; MCP module tests (if available)
(when (file-exists-p "test-claude-code-emacs-mcp-connection.el")
  (load-file "test-claude-code-emacs-mcp-connection.el")
  (load-file "test-claude-code-emacs-mcp-protocol.el")
  (load-file "test-claude-code-emacs-mcp-tools.el"))

;; Note: Old test files are no longer loaded to avoid duplicate test definitions
;; The tests have been split into module-specific files above

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
