;;; test-claude-code-ui.el --- Tests for UI, transient, and modes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-ui module

;;; Code:

(require 'ert)
(require 'claude-code-ui)
(require 'cl-lib)

;;; Tests for mode definitions

(ert-deftest test-claude-code-vterm-mode ()
  "Test vterm mode setup."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)

    ;; Test mode inheritance
    (should (derived-mode-p 'vterm-mode))

    ;; Test display settings
    (should (eq display-line-numbers-mode nil))))

;;; Tests for LSP integration

(ert-deftest test-claude-code-lsp-integration ()
  "Test LSP mode integration."
  (let ((lsp-language-id-configuration nil))
    (with-temp-buffer
      (claude-code-prompt-mode)
      (should (member '(claude-code-prompt-mode . "markdown")
                      lsp-language-id-configuration)))))

;;; Tests for transient menus

(ert-deftest test-claude-code-transient-defined ()
  "Test that transient menus are properly defined."
  (should (fboundp 'claude-code-transient))
  (should (fboundp 'claude-code-prompt-transient)))

(provide 'test-claude-code-ui)
;;; test-claude-code-ui.el ends here