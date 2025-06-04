;;; test-claude-code-emacs-core.el --- Tests for core utilities -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs-core module

;;; Code:

(require 'ert)
(require 'claude-code-emacs-core)
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

(ert-deftest test-claude-code-emacs-chunk-string ()
  "Test string chunking functionality."
  ;; Test normal chunking
  (let ((result (claude-code-emacs-chunk-string "Hello World" 5)))
    (should (equal result '("Hello" " Worl" "d"))))

  ;; Test empty string
  (let ((result (claude-code-emacs-chunk-string "" 5)))
    (should (equal result '(""))))

  ;; Test chunk size larger than string
  (let ((result (claude-code-emacs-chunk-string "Hi" 10)))
    (should (equal result '("Hi"))))

  ;; Test error on invalid chunk size
  (should-error (claude-code-emacs-chunk-string "test" 0))
  (should-error (claude-code-emacs-chunk-string "test" -1)))

(ert-deftest test-claude-code-emacs-count-arguments ()
  "Test counting $ARGUMENTS placeholders."
  (should (= 0 (claude-code-emacs-count-arguments "No arguments here")))
  (should (= 1 (claude-code-emacs-count-arguments "One $ARGUMENTS here")))
  (should (= 2 (claude-code-emacs-count-arguments "$ARGUMENTS and $ARGUMENTS")))
  (should (= 3 (claude-code-emacs-count-arguments "Start $ARGUMENTS middle $ARGUMENTS end $ARGUMENTS"))))

(provide 'test-claude-code-emacs-core)
;;; test-claude-code-emacs-core.el ends here
