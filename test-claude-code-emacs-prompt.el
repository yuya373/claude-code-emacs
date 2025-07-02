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

(ert-deftest test-claude-code-emacs-insert-region-path-to-prompt ()
  "Test inserting region path to prompt buffer."
  (let ((test-file "/home/user/project/src/main.el")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffers
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 1)  ; Go to line 2
      (set-mark (point))
      (forward-line 2)  ; Select lines 2-3
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-region-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify the path was inserted correctly with @ prefix and #L format
          (should (string-match-p "@src/main\\.el#L2-3" prompt-buffer-content))
          ;; Verify the content was inserted
          (should (string-match-p "line2\nline3\nline4" prompt-buffer-content))
          ;; Verify markdown code blocks were added
          (should (string-match-p "```" prompt-buffer-content))
          ;; Verify no duplicate newlines before closing ```
          (should-not (string-match-p "\n\n```" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-region-path-to-prompt-with-trailing-newline ()
  "Test inserting region path when content has trailing newline."
  (let ((test-file "/home/user/project/src/main.el")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffers
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\n")  ; Content with trailing newline
      (goto-char (point-min))
      (forward-line 1)  ; Go to line 2
      (set-mark (point))
      (goto-char (point-max))  ; Select to end including newline
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-region-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify no duplicate newlines before closing ```
          (should-not (string-match-p "\n\n```" prompt-buffer-content))
          ;; Verify single newline before closing ```
          (should (string-match-p "line3\n```" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-prompt ()
  "Test inserting current file @-path to prompt buffer."
  (let ((test-file "/home/user/project/docs/MCP-SETUP.md")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "# MCP Setup Documentation")
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify the @-path was inserted correctly
          (should (string-match-p "@docs/MCP-SETUP\\.md" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-prompt-with-region ()
  "Test inserting current file @-path with line range when region is selected."
  (let ((test-file "/home/user/project/src/utils.el")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 1)  ; Go to line 2
      (set-mark (point))
      (forward-line 1)  ; Move to line 3
      (end-of-line)     ; Move to end of line 3
      (activate-mark)
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify the @-path with line range was inserted correctly
          (should (string-match-p "@src/utils\\.el#L2-3" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-prompt-with-region-ending-newline ()
  "Test inserting current file @-path with line range when region ends with newline."
  (let ((test-file "/home/user/project/src/utils.el")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 1)  ; Go to line 2
      (set-mark (point))
      (forward-line 3)  ; Move to start of line 5 (after line 4's newline)
      (activate-mark)
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify the @-path with line range was inserted correctly
          ;; Should be L2-4, not L2-5, since region ends at start of line 5
          (should (string-match-p "@src/utils\\.el#L2-4" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-prompt-with-single-line-region ()
  "Test inserting current file @-path with single line selection."
  (let ((test-file "/home/user/project/src/utils.el")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 2)  ; Go to line 3
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)     ; Select only line 3
      (activate-mark)
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify the @-path with single line number was inserted correctly
          ;; Should be L3, not L3-3
          (should (string-match-p "@src/utils\\.el#L3\\b" prompt-buffer-content))
          (should-not (string-match-p "@src/utils\\.el#L3-3" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-region-path-to-prompt-single-line ()
  "Test inserting region path to prompt buffer with single line selection."
  (let ((test-file "/home/user/project/src/main.el")
        (prompt-file "/home/user/project/.claude-code-emacs.prompt.md")
        (prompt-buffer-content nil)
        (test-prompt-buffer nil))
    ;; Create test buffers
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 2)  ; Go to line 3
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)     ; Select only line 3
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-open-prompt-file)
                  (lambda ()
                    (setq test-prompt-buffer (get-buffer-create "*test-prompt*"))
                    (switch-to-buffer test-prompt-buffer)
                    (setq buffer-file-name prompt-file)
                    (claude-code-emacs-prompt-mode))))
        
        ;; Call the function
        (claude-code-emacs-insert-region-path-to-prompt)
        
        ;; Check the result
        (when test-prompt-buffer
          (setq prompt-buffer-content
                (with-current-buffer test-prompt-buffer
                  (prog1 (buffer-string)
                    (kill-buffer))))
          
          ;; Verify the path was inserted correctly with single line format
          (should (string-match-p "@src/main\\.el#L3\\b" prompt-buffer-content))
          (should-not (string-match-p "@src/main\\.el#L3-3" prompt-buffer-content))
          ;; Verify the content was inserted
          (should (string-match-p "line3" prompt-buffer-content))
          ;; Verify markdown code blocks were added
          (should (string-match-p "```" prompt-buffer-content)))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-session ()
  "Test inserting current file @-path directly to Claude Code session."
  (let ((test-file "/home/user/project/src/main.el")
        (sent-string nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "# Main file content")
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-send-string)
                  (lambda (string &optional _)
                    (setq sent-string string))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-session)
        
        ;; Check the result
        (should (equal sent-string "@src/main.el"))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-session-with-region ()
  "Test inserting current file @-path with line range directly to Claude Code session."
  (let ((test-file "/home/user/project/src/utils.el")
        (sent-string nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 1)  ; Go to line 2
      (set-mark (point))
      (forward-line 2)  ; Select lines 2-3
      (activate-mark)
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-send-string)
                  (lambda (string &optional _)
                    (setq sent-string string))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-session)
        
        ;; Check the result
        (should (equal sent-string "@src/utils.el#L2-3"))))))

(ert-deftest test-claude-code-emacs-insert-current-file-path-to-session-single-line ()
  "Test inserting current file @-path with single line to Claude Code session."
  (let ((test-file "/home/user/project/README.md")
        (sent-string nil))
    ;; Create test buffer
    (with-temp-buffer
      (setq buffer-file-name test-file)
      (insert "line1\nline2\nline3\nline4\nline5")
      (goto-char (point-min))
      (forward-line 2)  ; Go to line 3
      (beginning-of-line)
      (set-mark (point))
      (end-of-line)     ; Select only line 3
      (activate-mark)
      
      ;; Mock functions
      (cl-letf* (((symbol-function 'projectile-project-root)
                  (lambda (&optional _) "/home/user/project/"))
                 ((symbol-function 'claude-code-emacs-send-string)
                  (lambda (string &optional _)
                    (setq sent-string string))))
        
        ;; Call the function
        (claude-code-emacs-insert-current-file-path-to-session)
        
        ;; Check the result
        (should (equal sent-string "@README.md#L3"))))))

(provide 'test-claude-code-emacs-prompt)
;;; test-claude-code-emacs-prompt.el ends here
