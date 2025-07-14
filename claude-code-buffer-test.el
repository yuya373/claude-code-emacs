;;; test-claude-code-buffer.el --- Tests for buffer management and key sending -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-buffer module

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

(defmacro with-claude-mock-buffer (&rest body)
  "Execute BODY with a mock Claude Code buffer."
  `(with-claude-test-project
    (let ((buffer-name (claude-code-buffer-name)))
      (with-current-buffer (get-buffer-create buffer-name)
        (claude-code-vterm-mode)
        ,@body)
      (when (get-buffer buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer-name))))))

;;; Tests for buffer management

(ert-deftest test-claude-code-buffer-name ()
  "Test buffer name generation."
  (with-claude-test-project
   (should (string-match-p "\\*claude:.*\\*" (claude-code-buffer-name)))
   (should (string-match-p (regexp-quote temp-dir) (claude-code-buffer-name)))))

(ert-deftest test-claude-code-switch-to-buffer ()
  "Test switching to Claude Code buffer."
  (with-claude-mock-buffer
   (let ((buffer-name (claude-code-buffer-name)))
     ;; Test switching to existing buffer
     (with-temp-buffer
       (claude-code-switch-to-buffer)
       (should (string= (buffer-name) buffer-name)))

     ;; Test when buffer doesn't exist
     (let ((kill-buffer-query-functions nil))
       (kill-buffer buffer-name))
     (with-temp-buffer
       ;; Mock y-or-n-p to return nil (user says no)
       (cl-letf (((symbol-function 'y-or-n-p)
                  (lambda (_prompt) nil)))
         (let ((inhibit-message t))
           (claude-code-switch-to-buffer))
         ;; Should stay in current buffer when user declines
         (should-not (string= (buffer-name) buffer-name)))))))

;;; Tests for send functions with mock vterm

(ert-deftest test-claude-code-send-string-mock ()
  "Test send-string with mocked vterm functions."
  (cl-letf* ((vterm-strings nil)
             ((symbol-function 'vterm-send-string)
              (lambda (str &optional paste-p)
                (push str vterm-strings))))

    (with-claude-mock-buffer
     ;; Test normal send
     (claude-code-send-string "test command")
     (should (member "test command" vterm-strings)))))

(ert-deftest test-claude-code-special-key-functions ()
  "Test special key sending functions."
  (cl-letf* ((escape-sent nil)
             (return-sent nil)
             (ctrl-r-sent nil)
             ((symbol-function 'vterm-send-escape)
              (lambda () (setq escape-sent t)))
             ((symbol-function 'vterm-send-return)
              (lambda () (setq return-sent t)))
             ((symbol-function 'vterm-send-key)
              (lambda (key &optional shift)
                ;; Check if the key is Ctrl-R (character code 18)
                (when (and (stringp key) (= (aref key 0) ?\C-r))
                  (setq ctrl-r-sent t))))
             ((symbol-function 'kbd)
              (lambda (key-string)
                (cond ((string= key-string "C-r") "\C-r")
                      ((string= key-string "<backtab>") "<backtab>")
                      (t key-string)))))

    (with-claude-mock-buffer
     ;; Test escape sending
     (claude-code-send-escape)
     (should escape-sent)

     ;; Test return sending
     (claude-code-send-return)
     (should return-sent)

     ;; Test ctrl-r sending
     (claude-code-send-ctrl-r)
     (should ctrl-r-sent))))

;;; Tests for quick send functions

(ert-deftest test-claude-code-quick-send-functions ()
  "Test quick send functions."
  (cl-letf* ((sent-commands nil)
             ((symbol-function 'claude-code-send-string)
              (lambda (str &optional paste-p)
                (push str sent-commands))))

    ;; Test number sending functions
    (claude-code-send-1)
    (should (member "1" sent-commands))

    (claude-code-send-2)
    (should (member "2" sent-commands))

    (claude-code-send-3)
    (should (member "3" sent-commands))

    ;; Test commit sending
    (claude-code-send-commit)
    (should (member "commit" sent-commands))))




(ert-deftest test-claude-code-at-sign-complete ()
  "Test @ completion functionality."
  (cl-letf* ((selected-file nil)
             ((symbol-function 'completing-read)
              (lambda (prompt choices &rest args)
                (setq selected-file (car choices))
                selected-file))
             ((symbol-function 'projectile-project-files)
              (lambda (root)
                '("test.el" "src/main.el"))))

    (with-claude-test-project
     ;; Test @ completion
     (with-temp-buffer
       (claude-code-at-sign-complete)
       (should (equal (buffer-string) "@test.el"))))))

(provide 'test-claude-code-buffer)
;;; test-claude-code-buffer.el ends here
