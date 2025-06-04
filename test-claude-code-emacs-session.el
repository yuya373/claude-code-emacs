;;; test-claude-code-emacs-session.el --- Tests for session management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs-session module

;;; Code:

(require 'ert)
(require 'claude-code-emacs-session)
(require 'cl-lib)

;; Disable MCP auto-connect for tests
(when (featurep 'claude-code-emacs-mcp)
  (setq claude-code-emacs-mcp-auto-connect nil))

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

;;; Tests for session management

(ert-deftest test-claude-code-emacs-close ()
  "Test closing Claude Code window."
  (with-claude-test-project
   ;; Mock projectile-project-root to return our temp-dir
   (cl-letf (((symbol-function 'projectile-project-root)
              (lambda () temp-dir)))
     (let* ((buffer-name (format "*claude:%s*" temp-dir))
            (test-buffer (get-buffer-create buffer-name)))
       ;; Create a Claude Code buffer without vterm process
       (with-current-buffer test-buffer
         ;; Just set the mode name without invoking vterm
         (setq major-mode 'claude-code-emacs-vterm-mode)
         (setq mode-name "Claude Code Session"))

       ;; Test closing - should only close windows, not kill buffer
       (let ((inhibit-message nil))
         ;; Mock claude-code-emacs-get-buffer to return our test buffer
         (cl-letf (((symbol-function 'claude-code-emacs-get-buffer)
                    (lambda () test-buffer))
                   ((symbol-function 'get-buffer-window-list)
                    (lambda (buffer &optional minibuf all-frames)
                      (message "get-buffer-window-list called for: %s" buffer)
                      nil))) ; No windows in batch mode
           (message "Before claude-code-emacs-close, buffer alive: %s" (buffer-live-p test-buffer))
           (claude-code-emacs-close)
           (message "After claude-code-emacs-close, buffer alive: %s" (buffer-live-p test-buffer))))
       
       ;; Buffer should still exist after closing
       (should (buffer-live-p test-buffer))

       ;; Clean up the buffer
       (when (buffer-live-p test-buffer)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer test-buffer)))

       ;; Test closing when no buffer exists
       (cl-letf (((symbol-function 'claude-code-emacs-get-buffer)
                  (lambda () nil)))
         (let ((inhibit-message t))
           (claude-code-emacs-close)))
       ;; Should not error when no buffer exists
       (should t)))))

(ert-deftest test-claude-code-emacs-quit ()
  "Test quitting Claude Code session."
  (with-claude-test-project
   ;; Mock projectile-project-root and claude-code-emacs-get-buffer
   (cl-letf (((symbol-function 'projectile-project-root)
              (lambda () temp-dir))
             ((symbol-function 'claude-code-emacs-get-buffer)
              (lambda () (get-buffer (format "*claude:%s*" temp-dir)))))
     (let ((buffer-name (format "*claude:%s*" temp-dir))
           (kill-buffer-after-timer nil))
       ;; Create a Claude Code buffer with mock vterm process
       (with-current-buffer (get-buffer-create buffer-name)
         ;; Set up the mode and mock process
         (setq major-mode 'claude-code-emacs-vterm-mode)
         (setq mode-name "Claude Code Session")
         ;; Mock vterm process
         (setq-local vterm--process 'dummy-process))

       ;; Mock vterm-send functions to track what was sent
       (let ((sent-strings '())
             (return-sent nil))
         (cl-letf (((symbol-function 'vterm-send-string)
                    (lambda (str)
                      (push str sent-strings)))
                   ((symbol-function 'vterm-send-return)
                    (lambda ()
                      (setq return-sent t)))
                   ((symbol-function 'run-at-time)
                    (lambda (delay repeat function)
                      ;; Instead of running later, save the function to run it now
                      (setq kill-buffer-after-timer function)))
                   ((symbol-function 'process-live-p)
                    (lambda (proc) t))
                   ((symbol-function 'kill-process)
                    (lambda (proc) nil))
                   ((symbol-function 'claude-code-emacs-mcp-disconnect)
                    (lambda (project-root) nil))
                   ((symbol-function 'claude-code-emacs-mcp-unregister-port)
                    (lambda (project-root) nil)))
           
           ;; Test quit function
           (let ((inhibit-message t))
             (claude-code-emacs-quit))
           
           ;; Should have sent /quit command
           (should (member "/quit" sent-strings))
           (should return-sent)
           
           ;; Execute the timer function now
           (when kill-buffer-after-timer
             (funcall kill-buffer-after-timer))
           
           ;; Buffer should be killed
           (should-not (get-buffer buffer-name))))
       
       ;; Clean up if needed
       (when (get-buffer buffer-name)
         (let ((kill-buffer-query-functions nil))
           (kill-buffer buffer-name)))))))

;;; Integration test

(ert-deftest test-claude-code-emacs-integration ()
  "Test integrated workflow."
  (with-claude-test-project
   ;; Create prompt file
   (claude-code-emacs-open-prompt-file)
   (goto-char (point-max))
   (insert "\n## Test Section\nThis is a test prompt.")
   (save-buffer)

   ;; Test section extraction
   (search-backward "This is a test prompt.")
   (let ((section (claude-code-emacs-get-markdown-section-at-point)))
     (should (string-match-p "Test Section" section))
     (should (string-match-p "test prompt" section)))

   ;; Clean up
   (let ((kill-buffer-query-functions nil))
     (kill-buffer (current-buffer)))))

(provide 'test-claude-code-emacs-session)
;;; test-claude-code-emacs-session.el ends here
