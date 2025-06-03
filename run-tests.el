;;; run-tests.el --- Test runner for claude-code-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Run this file to execute all tests:
;; emacs -batch -l run-tests.el

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))

;; Load dependencies
(require 'ert)
(require 'projectile)
(require 'vterm)
(require 'transient)
(require 'markdown-mode)

;; Load the package
(require 'claude-code-emacs)

;; Load tests
(load-file "test-claude-code-emacs.el")

;; Load MCP tests if available
(when (file-exists-p "test-claude-code-emacs-mcp.el")
  (when (require 'claude-code-emacs-mcp nil t)
    (load-file "test-claude-code-emacs-mcp.el")))

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
