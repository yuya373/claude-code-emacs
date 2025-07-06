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

;; Load all modules
(require 'claude-code-emacs)

;; Load test files in order
;; Core module tests
(load-file "claude-code-emacs-core-test.el")
(load-file "claude-code-emacs-buffer-test.el")
(load-file "claude-code-emacs-commands-test.el")
(load-file "claude-code-emacs-ui-test.el")
(load-file "claude-code-emacs-prompt-test.el")

;; MCP module tests (if available)
(when (file-exists-p "claude-code-emacs-mcp-connection-test.el")
  (load-file "claude-code-emacs-mcp-connection-test.el")
  (load-file "claude-code-emacs-mcp-protocol-test.el")
  (load-file "claude-code-emacs-mcp-tools-test.el")
  (load-file "claude-code-emacs-mcp-events-test.el"))

;; Note: Old test files are no longer loaded to avoid duplicate test definitions
;; The tests have been split into module-specific files above

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
