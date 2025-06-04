;;; test-claude-code-emacs-prompt.el --- Tests for prompt file functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs-prompt module

;;; Code:

(require 'ert)
(require 'claude-code-emacs-prompt)
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

;;; Tests for prompt file functions

(ert-deftest test-claude-code-emacs-open-prompt-file ()
  "Test prompt file creation and opening."
  (with-claude-test-project
   (let* ((prompt-file (expand-file-name ".claude-code-emacs.prompt.md" temp-dir))
          (original-buffer (current-buffer)))

     ;; Test file creation
     (should-not (file-exists-p prompt-file))
     (claude-code-emacs-open-prompt-file)
     (should (file-exists-p prompt-file))

     ;; Test file content
     (with-temp-buffer
       (insert-file-contents prompt-file)
       (should (string-match-p "# Claude Code Prompts for" (buffer-string)))
       (should (string-match-p "## Project Context" (buffer-string)))
       (should (string-match-p "## Common Tasks" (buffer-string)))
       (should (string-match-p "## Code Patterns" (buffer-string))))

     ;; Test that buffer is in correct mode
     (should (eq major-mode 'claude-code-emacs-prompt-mode))

     ;; Clean up
     (let ((kill-buffer-query-functions nil))
       (kill-buffer (current-buffer))))))

(ert-deftest test-claude-code-emacs-get-markdown-section-at-point ()
  "Test markdown section extraction."
  (with-temp-buffer
    (insert "# Section 1\n")
    (insert "Content 1\n")
    (insert "## Section 2\n")
    (insert "Content 2\n")
    (insert "### Section 3\n")
    (insert "Content 3\n")
    (markdown-mode)

    ;; Test at heading
    (goto-char (point-min))
    (let ((section (claude-code-emacs-get-markdown-section-at-point)))
      (should (string-match-p "# Section 1" section))
      (should (string-match-p "Content 1" section))
      (should-not (string-match-p "## Section 2" section)))

    ;; Test in content
    (search-forward "Content 2")
    (let ((section (claude-code-emacs-get-markdown-section-at-point)))
      (should (string-match-p "## Section 2" section))
      (should (string-match-p "Content 2" section))
      (should-not (string-match-p "### Section 3" section)))

    ;; Test last section
    (goto-char (point-max))
    (let ((section (claude-code-emacs-get-markdown-section-at-point)))
      (should (string-match-p "### Section 3" section))
      (should (string-match-p "Content 3" section)))))

;;; Tests for prompt mode

(ert-deftest test-claude-code-emacs-prompt-mode ()
  "Test prompt mode setup."
  (with-temp-buffer
    (claude-code-emacs-prompt-mode)

    ;; Test mode inheritance
    (should (derived-mode-p 'markdown-mode))

    ;; Test keybindings
    (should (eq (lookup-key claude-code-emacs-prompt-mode-map (kbd "C-c C-s"))
                'claude-code-emacs-send-prompt-at-point))
    (should (eq (lookup-key claude-code-emacs-prompt-mode-map (kbd "C-c C-r"))
                'claude-code-emacs-send-prompt-region))
    (should (eq (lookup-key claude-code-emacs-prompt-mode-map (kbd "C-c C-o"))
                'claude-code-emacs-run))
    (should (eq (lookup-key claude-code-emacs-prompt-mode-map (kbd "C-c C-t"))
                'claude-code-emacs-prompt-transient))))

(provide 'test-claude-code-emacs-prompt)
;;; test-claude-code-emacs-prompt.el ends here
