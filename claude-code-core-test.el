;;; test-claude-code-core.el --- Tests for core utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-core module

;;; Code:

(require 'ert)
(require 'claude-code-core)
(require 'cl-lib)

;;; Test utilities

(defmacro with-claude-test-project (&rest body)
  "Execute BODY in a temporary project directory."
  `(let* ((temp-dir (make-temp-file "claude-test-" t))
          (default-directory temp-dir)
          (projectile-project-root temp-dir))
     (unwind-protect
         (progn
           ;; Create a minimal .git directory to make projectile recognize it as a project
           (make-directory (expand-file-name ".git" temp-dir))
           ,@body)
       (delete-directory temp-dir t))))

;;; Tests for core functions

(ert-deftest test-claude-code-count-arguments ()
  "Test counting $ARGUMENTS placeholders."
  (should (= 0 (claude-code-count-arguments "No arguments here")))
  (should (= 1 (claude-code-count-arguments "One $ARGUMENTS here")))
  (should (= 2 (claude-code-count-arguments "$ARGUMENTS and $ARGUMENTS")))
  (should (= 3 (claude-code-count-arguments "Start $ARGUMENTS middle $ARGUMENTS end $ARGUMENTS"))))

(ert-deftest test-claude-code-send-region ()
  "Test sending selected region to Claude Code."
  ;; Mock the required functions
  (cl-letf* ((sent-text nil)
             ((symbol-function 'claude-code-send-string)
              (lambda (text) (setq sent-text text)))
             ((symbol-function 'use-region-p) (lambda () t))
             ((symbol-function 'region-beginning) (lambda () 1))
             ((symbol-function 'region-end) (lambda () 6)))
    ;; Test with region selected
    (with-temp-buffer
      (insert "Hello World")
      (claude-code-send-region)
      (should (equal sent-text "Hello"))))

  ;; Test without region selected
  (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
    (should-error (claude-code-send-region) :type 'user-error)))

(ert-deftest test-claude-code-run ()
  "Test starting Claude Code session."
  (with-claude-test-project
    (let* ((buffer-created nil)
           (buffer-switched nil)
           (created-buffer-name nil)
           (vterm-shell-value nil)
           (vterm-mode-called nil)
           (captured-default-directory nil)
           (test-buffer (generate-new-buffer "*test-buffer*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'get-buffer-create)
                      (lambda (name &rest _)
                        (setq buffer-created t)
                        (setq created-buffer-name name)
                        ;; A real `get-buffer-create' gives the new buffer the
                        ;; current `default-directory'; capture it here to
                        ;; verify what the Claude buffer would inherit.
                        (setq captured-default-directory default-directory)
                        ;; Simulate vterm buffer
                        (with-current-buffer test-buffer
                          (setq-local major-mode 'vterm-mode)
                          (current-buffer))))
                     ((symbol-function 'switch-to-buffer-other-window)
                      (lambda (name &rest _)
                        (setq buffer-switched t)))
                     ((symbol-function 'claude-code-vterm-mode)
                      (lambda ()
                        (setq vterm-mode-called t)
                        ;; Mock vterm-mode setup
                        (setq-local vterm-shell claude-code-executable)
                        (setq major-mode 'claude-code-vterm-mode)))
                     ((symbol-function 'vterm)
                      (lambda (buffer-name)
                        ;; Return the test buffer to simulate vterm creation
                        test-buffer))
                     (current-prefix-arg nil))
            ;; Test basic run without prefix argument
            (claude-code-run)
            (should buffer-created)
            (should buffer-switched)
            (should (string-match-p "\\*claude:" created-buffer-name))
            (should vterm-mode-called)
            ;; `default-directory' must keep its trailing slash so commands run
            ;; from the Claude buffer (find-file, compile, ...) resolve paths
            ;; against the project root correctly.
            (should captured-default-directory)
            (should (directory-name-p captured-default-directory)))
        (kill-buffer test-buffer)))))

(ert-deftest test-claude-code-run-with-options ()
  "Test starting Claude Code session with interactive options."
  (with-claude-test-project
    (let* ((buffer-created nil)
           (buffer-switched nil)
           (vterm-shell-value nil)
           (vterm-mode-called nil)
           (test-buffer (generate-new-buffer "*test-buffer*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'get-buffer-create)
                      (lambda (name &rest _)
                        (setq buffer-created t)
                        ;; Simulate vterm buffer
                        (with-current-buffer test-buffer
                          (setq-local major-mode 'vterm-mode)
                          (current-buffer))))
                     ((symbol-function 'switch-to-buffer-other-window)
                      (lambda (name &rest _)
                        (setq buffer-switched t)))
                     ((symbol-function 'claude-code-vterm-mode)
                      (lambda ()
                        (setq vterm-mode-called t)
                        ;; Capture the vterm-shell value that was set
                        (setq vterm-shell-value vterm-shell)
                        (setq major-mode 'claude-code-vterm-mode)))
                     ((symbol-function 'vterm)
                      (lambda (buffer-name)
                        ;; Return the test buffer to simulate vterm creation
                        test-buffer))
                     ((symbol-function 'completing-read)
                      (lambda (prompt choices &rest _)
                        "--model sonnet - Use Claude Sonnet model"))
                     (current-prefix-arg t))
            ;; Test run with prefix argument for option selection
            (claude-code-run)
            (should buffer-created)
            (should buffer-switched)
            (should vterm-mode-called)
            ;; Check that vterm-shell includes the selected option
            (should (string-match-p "--model sonnet" vterm-shell-value)))
        (kill-buffer test-buffer)))))

(ert-deftest test-claude-code-run-with-resume ()
  "Test starting Claude Code session with resume option."
  (with-claude-test-project
    (let* ((buffer-created nil)
           (vterm-shell-value nil)
           (vterm-mode-called nil)
           (test-buffer (generate-new-buffer "*test-buffer*")))
      (unwind-protect
          (cl-letf* (((symbol-function 'get-buffer-create)
                      (lambda (name &rest _)
                        (setq buffer-created t)
                        ;; Simulate vterm buffer
                        (with-current-buffer test-buffer
                          (setq-local major-mode 'vterm-mode)
                          (current-buffer))))
                     ((symbol-function 'switch-to-buffer-other-window)
                      (lambda (name &rest _) nil))
                     ((symbol-function 'claude-code-vterm-mode)
                      (lambda ()
                        (setq vterm-mode-called t)
                        ;; Capture the vterm-shell value that was set
                        (setq vterm-shell-value vterm-shell)
                        (setq major-mode 'claude-code-vterm-mode)))
                     ((symbol-function 'vterm)
                      (lambda (buffer-name)
                        ;; Return the test buffer to simulate vterm creation
                        test-buffer))
                     ((symbol-function 'completing-read)
                      (lambda (prompt choices &rest _)
                        "--resume - Resume specific session by ID"))
                     (current-prefix-arg t))
            ;; Test run with resume option (no session ID prompt)
            (claude-code-run)
            (should buffer-created)
            (should vterm-mode-called)
            ;; Check that vterm-shell includes the resume option only
            (should (string-match-p "--resume" vterm-shell-value))
            ;; Session ID should not be in the command (claude handles it automatically)
            (should-not (string-match-p "test-session" vterm-shell-value)))
        (kill-buffer test-buffer)))))

(ert-deftest test-claude-code-normalize-project-root ()
  (should (equal "/foo/bar/baz" (claude-code-normalize-project-root "/foo/bar/baz/")))
  (should-error (claude-code-normalize-project-root nil) :type 'error))

(ert-deftest test-claude-code--wait-for-vterm ()
  "Test the vterm wait helper, including a nil `vterm-timer-delay'."
  (let ((waited nil))
    (cl-letf (((symbol-function 'sit-for)
               (lambda (seconds &rest _) (setq waited seconds) t)))
      ;; Custom delay: waits three times the configured delay
      (let ((vterm-timer-delay 0.1))
        (claude-code--wait-for-vterm)
        (should (= waited (* 0.1 3))))
      ;; nil delay (vterm's "update immediately"): must not signal,
      ;; and still waits some positive amount
      (let ((vterm-timer-delay nil))
        (setq waited nil)
        (claude-code--wait-for-vterm)
        (should (numberp waited))
        (should (> waited 0))))))

(provide 'test-claude-code-core)
;;; test-claude-code-core.el ends here
