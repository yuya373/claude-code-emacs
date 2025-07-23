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

;;; Tests for vterm multiline buffer filter

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-disabled ()
  "Test vterm multiline buffer filter when feature is disabled."
  (let ((orig-fun-called nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output nil))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer))))
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (setq orig-fun-called t))
           test-process
           "test input")
          (should orig-fun-called))))))

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-simple-input ()
  "Test vterm multiline buffer filter with simple input (no escape sequences)."
  (let ((orig-fun-called nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output t))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'claude-code-buffer-name) (lambda () (buffer-name))))
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (setq orig-fun-called t))
           test-process
           "simple text")
          (should orig-fun-called))))))

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-multiline ()
  "Test vterm multiline buffer filter with escape sequences."
  (let ((orig-fun-called nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output t)
            (claude-code-vterm-multiline-delay 0.001))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'claude-code-buffer-name) (lambda () (buffer-name)))
                  ((symbol-function 'get-buffer-process) (lambda (_) test-process))
                  ((symbol-function 'process-live-p) (lambda (_) t)))
          ;; Input with multiple escape sequences
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (setq orig-fun-called t))
           test-process
           "\033[K\033[1;1H\033[2A\033[3B")
          ;; Should not be called immediately
          (should-not orig-fun-called)
          ;; Should have set up buffer
          (should claude-code--vterm-multiline-buffer)
          ;; Wait for timer
          (sleep-for 0.01)
          ;; Now it should have been called
          (should orig-fun-called))))))

(ert-deftest test-claude-code--vterm-cleanup-multiline-timer ()
  "Test vterm multiline timer cleanup function."
  (with-temp-buffer
    ;; Set up some test state
    (setq-local claude-code--vterm-multiline-buffer "test buffer content")
    (setq-local claude-code--vterm-multiline-buffer-timer
                (run-at-time 10 nil (lambda () nil))) ; Timer that won't fire

    ;; Call cleanup
    (claude-code--vterm-cleanup-multiline-timer)

    ;; Check that everything is cleaned up
    (should-not claude-code--vterm-multiline-buffer)
    (should-not claude-code--vterm-multiline-buffer-timer)))

(ert-deftest test-claude-code--vterm-multiline-error-handling ()
  "Test error handling in vterm multiline buffer filter during delayed processing."
  (let ((error-message nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output t)
            (claude-code-vterm-multiline-delay 0.001))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'claude-code-buffer-name) (lambda () (buffer-name)))
                  ((symbol-function 'get-buffer-process) (lambda (_) test-process))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq error-message (apply #'format fmt args)))))
          ;; Process multiline input with error-throwing function
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (error "Test error"))
           test-process
           "\033[K\033[1;1H\033[2A\033[3B") ; Multiple escape sequences to trigger buffering
          ;; Wait for timer to process
          (sleep-for 0.01)
          ;; Should have captured error message
          (should error-message)
          (should (string-match "Error in vterm filter:" error-message))
          (should (string-match "Test error" error-message)))))))

;;; Tests for transient menus

(ert-deftest test-claude-code-transient-defined ()
  "Test that transient menus are properly defined."
  (should (fboundp 'claude-code-transient))
  (should (fboundp 'claude-code-prompt-transient)))

(provide 'test-claude-code-ui)
;;; test-claude-code-ui.el ends here
