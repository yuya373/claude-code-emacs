;;; test-claude-code-emacs-ui.el --- Tests for UI, transient, and modes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs-ui module

;;; Code:

(require 'ert)
(require 'claude-code-emacs-ui)
(require 'cl-lib)

;;; Tests for mode definitions

(ert-deftest test-claude-code-emacs-vterm-mode ()
  "Test vterm mode setup."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-emacs-vterm-mode)

    ;; Test mode inheritance
    (should (derived-mode-p 'vterm-mode))

    ;; Test display settings
    (should (eq display-line-numbers-mode nil))))

;;; Tests for LSP integration

(ert-deftest test-claude-code-emacs-lsp-integration ()
  "Test LSP mode integration."
  (let ((lsp-language-id-configuration nil))
    (with-temp-buffer
      (claude-code-emacs-prompt-mode)
      (should (member '(claude-code-emacs-prompt-mode . "markdown")
                      lsp-language-id-configuration)))))

;;; Tests for transient menus

(ert-deftest test-claude-code-emacs-transient-defined ()
  "Test that transient menus are properly defined."
  (should (fboundp 'claude-code-emacs-transient))
  (should (fboundp 'claude-code-emacs-prompt-transient)))

(provide 'test-claude-code-emacs-ui)
;;; test-claude-code-emacs-ui.el ends here