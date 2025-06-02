;;; test-claude-code-emacs.el --- Tests for claude-code-emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs package

;;; Code:

(require 'ert)
(require 'claude-code-emacs)
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
    (let ((buffer-name (claude-code-emacs-buffer-name)))
      (with-current-buffer (get-buffer-create buffer-name)
        (claude-code-emacs-vterm-mode)
        ,@body)
      (when (get-buffer buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer-name))))))

;;; Tests for core functions

(ert-deftest test-claude-code-emacs-buffer-name ()
  "Test buffer name generation."
  (with-claude-test-project
   (should (string-match-p "\\*claude:.*\\*" (claude-code-emacs-buffer-name)))
   (should (string-match-p (regexp-quote temp-dir) (claude-code-emacs-buffer-name)))))

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

(ert-deftest test-claude-code-emacs-switch-to-buffer ()
  "Test switching to Claude Code buffer."
  (with-claude-mock-buffer
   (let ((buffer-name (claude-code-emacs-buffer-name)))
     ;; Test switching to existing buffer
     (with-temp-buffer
       (claude-code-emacs-switch-to-buffer)
       (should (string= (buffer-name) buffer-name)))

     ;; Test when buffer doesn't exist
     (let ((kill-buffer-query-functions nil))
       (kill-buffer buffer-name))
     (with-temp-buffer
       (let ((inhibit-message t))
         (claude-code-emacs-switch-to-buffer))
       ;; Should stay in current buffer when Claude buffer doesn't exist
       (should-not (string= (buffer-name) buffer-name))))))

(ert-deftest test-claude-code-emacs-close ()
  "Test closing Claude Code window."
  (with-claude-test-project
   (let ((buffer-name (claude-code-emacs-buffer-name)))
     ;; Create a Claude Code buffer without vterm process
     (with-current-buffer (get-buffer-create buffer-name)
       ;; Just set the mode name without invoking vterm
       (setq major-mode 'claude-code-emacs-vterm-mode)
       (setq mode-name "Claude Code Session"))

     ;; Test closing window when buffer is displayed
     (should (get-buffer buffer-name))
     ;; Display the buffer in a window
     (save-window-excursion
       (split-window)
       (switch-to-buffer buffer-name)
       (should (get-buffer-window buffer-name))
       (let ((inhibit-message t))
         (claude-code-emacs-close))
       ;; Window should be closed but buffer still exists
       (should-not (get-buffer-window buffer-name))
       (should (get-buffer buffer-name)))

     ;; Test closing when buffer exists but not displayed
     (let ((inhibit-message t))
       (claude-code-emacs-close))
     ;; Buffer should still exist
     (should (get-buffer buffer-name))

     ;; Clean up the buffer
     (let ((kill-buffer-query-functions nil))
       (kill-buffer buffer-name))

     ;; Test closing when no buffer exists
     (let ((inhibit-message t))
       (claude-code-emacs-close))
     ;; Should not error when no buffer exists
     (should t))))

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

;;; Tests for mode definitions

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

;;; Tests for send functions with mock vterm

(ert-deftest test-claude-code-emacs-send-string-mock ()
  "Test send-string with mocked vterm functions."
  (cl-letf* ((vterm-strings nil)
             ((symbol-function 'vterm-send-string)
              (lambda (str &optional paste-p)
                (push (cons str paste-p) vterm-strings)))
             ((symbol-function 'vterm-send-return)
              (lambda () (push '(return . nil) vterm-strings))))

    (with-claude-mock-buffer
     ;; Test normal send
     (claude-code-emacs-send-string "test command")
     (should (member '("test command" . nil) vterm-strings))
     (should (member '(return . nil) vterm-strings))

     ;; Test paste mode
     (setq vterm-strings nil)
     (claude-code-emacs-send-string "pasted text" t)
     (should (member '("pasted text" . t) vterm-strings)))))

(ert-deftest test-claude-code-emacs-slash-commands ()
  "Test slash command functions."
  (cl-letf* ((sent-commands nil)
             ((symbol-function 'claude-code-emacs-send-string)
              (lambda (str &optional paste-p)
                (push str sent-commands))))

    ;; Test simple commands
    (claude-code-emacs-init)
    (should (member "/init" sent-commands))

    (claude-code-emacs-clear)
    (should (member "/clear" sent-commands))

    (claude-code-emacs-help)
    (should (member "/help" sent-commands))

    ;; Test commands with optional arguments
    (claude-code-emacs-config "")
    (should (member "/config" sent-commands))

    (claude-code-emacs-config "theme dark")
    (should (member "/config theme dark" sent-commands))

    (claude-code-emacs-compact "")
    (should (member "/compact" sent-commands))

    (claude-code-emacs-compact "focus on tests")
    (should (member "/compact focus on tests" sent-commands))))

(ert-deftest test-claude-code-emacs-quick-send-functions ()
  "Test quick send functions."
  (cl-letf* ((sent-commands nil)
             ((symbol-function 'claude-code-emacs-send-string)
              (lambda (str &optional paste-p)
                (push str sent-commands))))

    ;; Test number sending functions
    (claude-code-emacs-send-1)
    (should (member "1" sent-commands))

    (claude-code-emacs-send-2)
    (should (member "2" sent-commands))

    (claude-code-emacs-send-3)
    (should (member "3" sent-commands))

    ;; Test commit sending
    (claude-code-emacs-send-commit)
    (should (member "commit" sent-commands))))

(ert-deftest test-claude-code-emacs-special-key-functions ()
  "Test special key sending functions."
  (cl-letf* ((escape-sent nil)
             (return-sent nil)
             (ctrl-r-sent nil)
             ((symbol-function 'vterm-send-escape)
              (lambda () (setq escape-sent t)))
             ((symbol-function 'vterm-send-return)
              (lambda () (setq return-sent t)))
             ((symbol-function 'vterm-send-key)
              (lambda (key) (when (equal key (kbd "C-r"))
                              (setq ctrl-r-sent t)))))

    (with-claude-mock-buffer
     ;; Test escape sending
     (claude-code-emacs-send-escape)
     (should escape-sent)

     ;; Test return sending
     (claude-code-emacs-send-return)
     (should return-sent)

     ;; Test ctrl-r sending
     (claude-code-emacs-send-ctrl-r)
     (should ctrl-r-sent))))

;;; Tests for buffer path functions

(ert-deftest test-claude-code-emacs-format-buffer-path ()
  "Test formatting buffer path with @ prefix."
  (with-claude-test-project
   ;; Test file in project
   (let ((test-file (expand-file-name "test.el" temp-dir)))
     (with-temp-file test-file
       (insert "test content"))
     (with-current-buffer (find-file-noselect test-file)
       (let ((result (claude-code-emacs-format-buffer-path (current-buffer))))
         (should (equal result "@test.el")))
       (kill-buffer)))

   ;; Test file in subdirectory
   (let ((sub-dir (expand-file-name "src" temp-dir)))
     (make-directory sub-dir)
     (let ((test-file (expand-file-name "main.el" sub-dir)))
       (with-temp-file test-file
         (insert "test content"))
       (with-current-buffer (find-file-noselect test-file)
         (let ((result (claude-code-emacs-format-buffer-path (current-buffer))))
           (should (equal result "@src/main.el")))
         (kill-buffer))))

   ;; Test buffer without file
   (with-temp-buffer
     (should-not (claude-code-emacs-format-buffer-path (current-buffer))))

   ;; Test file outside project
   (let ((outside-file "/tmp/test-outside.el"))
     (with-temp-file outside-file
       (insert "test content"))
     (with-current-buffer (find-file-noselect outside-file)
       (should-not (claude-code-emacs-format-buffer-path (current-buffer)))
       (kill-buffer))
     (delete-file outside-file))))

(ert-deftest test-claude-code-emacs-get-buffer-paths ()
  "Test getting all buffer paths with @ prefix."
  (with-claude-test-project
   ;; Create test files
   (let ((file1 (expand-file-name "file1.el" temp-dir))
         (file2 (expand-file-name "file2.el" temp-dir))
         (sub-dir (expand-file-name "lib" temp-dir)))
     (make-directory sub-dir)
     (let ((file3 (expand-file-name "util.el" sub-dir)))

       ;; Create files
       (with-temp-file file1 (insert "content1"))
       (with-temp-file file2 (insert "content2"))
       (with-temp-file file3 (insert "content3"))

       ;; Open files in buffers
       (let ((buf1 (find-file-noselect file1))
             (buf2 (find-file-noselect file2))
             (buf3 (find-file-noselect file3)))

         ;; Test get-buffer-paths
         (let ((paths (claude-code-emacs-get-buffer-paths)))
           (should (member "@file1.el" paths))
           (should (member "@file2.el" paths))
           (should (member "@lib/util.el" paths)))

         ;; Clean up
         (kill-buffer buf1)
         (kill-buffer buf2)
         (kill-buffer buf3))))))

;;; Tests for file path insertion functions

(ert-deftest test-claude-code-emacs-insert-file-path ()
  "Test inserting individual file paths."
  (cl-letf* ((selected-path nil)
             ((symbol-function 'completing-read)
              (lambda (prompt choices &rest args)
                (setq selected-path (car choices))
                selected-path)))

    (with-claude-test-project
     ;; Create test files
     (let ((file1 (expand-file-name "test.el" temp-dir)))
       (with-temp-file file1 (insert "content"))
       (let ((buf1 (find-file-noselect file1)))

         ;; Test in a temp buffer
         (with-temp-buffer
           (claude-code-emacs-insert-file-path)
           (should (equal (buffer-string) "@test.el")))

         ;; Clean up
         (kill-buffer buf1))))))

(ert-deftest test-claude-code-emacs-insert-open-buffer-paths ()
  "Test inserting open buffer file paths."
  (with-claude-test-project
   ;; Create test files
   (let ((file1 (expand-file-name "file1.el" temp-dir))
         (file2 (expand-file-name "file2.el" temp-dir)))
     (with-temp-file file1 (insert "content1"))
     (with-temp-file file2 (insert "content2"))

     (let ((buf1 (find-file-noselect file1))
           (buf2 (find-file-noselect file2)))

       ;; Test in a temp buffer
       (with-temp-buffer
         (claude-code-emacs-insert-open-buffer-paths)
         (let ((content (buffer-string)))
           (should (string-match-p "@file1\\.el" content))
           (should (string-match-p "@file2\\.el" content))
           (should (string-match-p "\n" content))))

       ;; Clean up
       (kill-buffer buf1)
       (kill-buffer buf2)))))

(ert-deftest test-claude-code-emacs-at-sign-complete ()
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
       (claude-code-emacs-at-sign-complete)
       (should (equal (buffer-string) "@test.el"))))))

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

;;; Tests for custom project command functions

(ert-deftest test-claude-code-emacs-custom-commands-directory ()
  "Test custom commands directory path generation."
  (with-claude-test-project
   (let ((commands-dir (claude-code-emacs-custom-commands-directory)))
     (should (string-match-p "\\.claude/commands$" commands-dir))
     (should (string-prefix-p temp-dir commands-dir)))))

(ert-deftest test-claude-code-emacs-list-custom-command-files ()
  "Test listing custom command files."
  (with-claude-test-project
   ;; Test when directory doesn't exist
   (should (null (claude-code-emacs-list-custom-command-files)))
   
   ;; Create commands directory and files
   (let ((commands-dir (claude-code-emacs-custom-commands-directory)))
     (make-directory commands-dir t)
     
     ;; Create some test command files
     (with-temp-file (expand-file-name "test1.md" commands-dir)
       (insert "test command 1"))
     (with-temp-file (expand-file-name "test2.md" commands-dir)
       (insert "test command 2"))
     (with-temp-file (expand-file-name "not-markdown.txt" commands-dir)
       (insert "not a command"))
     
     ;; Test listing
     (let ((files (claude-code-emacs-list-custom-command-files)))
       (should (= 2 (length files)))
       (should (member "test1.md" files))
       (should (member "test2.md" files))
       (should-not (member "not-markdown.txt" files))))))

(ert-deftest test-claude-code-emacs-read-custom-command-file ()
  "Test reading custom command file contents."
  (with-claude-test-project
   (let ((commands-dir (claude-code-emacs-custom-commands-directory)))
     (make-directory commands-dir t)
     
     ;; Create test file
     (with-temp-file (expand-file-name "test-command.md" commands-dir)
       (insert "  test command content  \n"))
     
     ;; Test reading
     (should (equal "test command content"
                    (claude-code-emacs-read-custom-command-file "test-command.md")))
     
     ;; Test non-existent file
     (should (null (claude-code-emacs-read-custom-command-file "non-existent.md"))))))

(ert-deftest test-claude-code-emacs-execute-custom-command ()
  "Test custom command execution functionality."
  (with-claude-mock-buffer
   (let ((commands-dir (claude-code-emacs-custom-commands-directory))
         (claude-code-emacs-send-string-called nil)
         (claude-code-emacs-send-string-arg nil))
     ;; Mock send-string to capture calls
     (cl-letf (((symbol-function 'claude-code-emacs-send-string)
                (lambda (str)
                  (setq claude-code-emacs-send-string-called t
                        claude-code-emacs-send-string-arg str))))
       
       ;; Test with no commands directory
       (claude-code-emacs-execute-custom-command)
       (should-not claude-code-emacs-send-string-called)
       
       ;; Create commands directory and file
       (make-directory commands-dir t)
       (with-temp-file (expand-file-name "test-cmd.md" commands-dir)
         (insert "execute this command"))
       
       ;; Mock completing-read to select our test file
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "test-cmd.md")))
         (claude-code-emacs-execute-custom-command)
         (should claude-code-emacs-send-string-called)
         (should (equal "execute this command" claude-code-emacs-send-string-arg)))))))

;;; Tests for global command functions

(ert-deftest test-claude-code-emacs-global-commands-directory ()
  "Test global commands directory path generation."
  (let ((commands-dir (claude-code-emacs-global-commands-directory)))
    (should (string-match-p "/.claude/commands$" commands-dir))
    (should (string-prefix-p (expand-file-name "~") commands-dir))))

(ert-deftest test-claude-code-emacs-list-global-command-files ()
  "Test listing global command files."
  ;; This test is limited because we can't easily mock the home directory
  ;; We'll just test that the function doesn't error
  (let ((result (claude-code-emacs-list-global-command-files)))
    (should (or (null result) (listp result)))))

(ert-deftest test-claude-code-emacs-read-global-command-file ()
  "Test reading global command file contents."
  ;; This test is limited because we can't easily mock the home directory
  ;; We'll just test that the function doesn't error with non-existent file
  (should (null (claude-code-emacs-read-global-command-file "non-existent.txt"))))

(ert-deftest test-claude-code-emacs-execute-global-command ()
  "Test global command execution functionality."
  (with-claude-mock-buffer
   (let ((claude-code-emacs-send-string-called nil)
         (claude-code-emacs-send-string-arg nil))
     ;; Mock send-string to capture calls
     (cl-letf (((symbol-function 'claude-code-emacs-send-string)
                (lambda (str)
                  (setq claude-code-emacs-send-string-called t
                        claude-code-emacs-send-string-arg str)))
               ;; Mock list-global-command-files to return test data
               ((symbol-function 'claude-code-emacs-list-global-command-files)
                (lambda () '("test-command.md" "another-command.txt")))
               ;; Mock read-global-command-file to return content without $ARGUMENTS
               ((symbol-function 'claude-code-emacs-read-global-command-file)
                (lambda (file) "Simple command content"))
               ;; Mock completing-read to select a file
               ((symbol-function 'completing-read)
                (lambda (&rest _) "test-command.md")))
       
       (claude-code-emacs-execute-global-command)
       (should claude-code-emacs-send-string-called)
       (should (equal "/user:test-command.md" claude-code-emacs-send-string-arg))))))

;;; Tests for argument handling functions

(ert-deftest test-claude-code-emacs-count-arguments ()
  "Test counting $ARGUMENTS placeholders."
  (should (= 0 (claude-code-emacs-count-arguments "No arguments here")))
  (should (= 1 (claude-code-emacs-count-arguments "One $ARGUMENTS here")))
  (should (= 2 (claude-code-emacs-count-arguments "$ARGUMENTS and $ARGUMENTS")))
  (should (= 3 (claude-code-emacs-count-arguments "Start $ARGUMENTS middle $ARGUMENTS end $ARGUMENTS"))))


(ert-deftest test-claude-code-emacs-execute-custom-command-with-multiple-args ()
  "Test custom command execution with multiple $ARGUMENTS."
  (with-claude-mock-buffer
   (let ((commands-dir (claude-code-emacs-custom-commands-directory))
         (claude-code-emacs-send-string-called nil)
         (claude-code-emacs-send-string-arg nil))
     ;; Mock send-string to capture calls
     (cl-letf (((symbol-function 'claude-code-emacs-send-string)
                (lambda (str)
                  (setq claude-code-emacs-send-string-called t
                        claude-code-emacs-send-string-arg str))))
       
       ;; Create commands directory and test file
       (make-directory commands-dir t)
       (with-temp-file (expand-file-name "multi-arg.md" commands-dir)
         (insert "Command with $ARGUMENTS and $ARGUMENTS"))
       
       ;; Mock user input
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "multi-arg.md"))
                 ((symbol-function 'read-string)
                  (let ((counter 0))
                    (lambda (&rest _)
                      (setq counter (1+ counter))
                      (format "arg%d" counter)))))
         
         (claude-code-emacs-execute-custom-command)
         (should claude-code-emacs-send-string-called)
         (should (equal "/project:multi-arg arg1 arg2" claude-code-emacs-send-string-arg)))))))

(ert-deftest test-claude-code-emacs-execute-global-command-with-multiple-args ()
  "Test global command execution with multiple $ARGUMENTS."
  (with-claude-mock-buffer
   (let ((claude-code-emacs-send-string-called nil)
         (claude-code-emacs-send-string-arg nil))
     ;; Mock send-string to capture calls
     (cl-letf (((symbol-function 'claude-code-emacs-send-string)
                (lambda (str)
                  (setq claude-code-emacs-send-string-called t
                        claude-code-emacs-send-string-arg str)))
               ;; Mock list-global-command-files
               ((symbol-function 'claude-code-emacs-list-global-command-files)
                (lambda () '("multi-cmd.txt")))
               ;; Mock read-global-command-file
               ((symbol-function 'claude-code-emacs-read-global-command-file)
                (lambda (file) "Do $ARGUMENTS with $ARGUMENTS and $ARGUMENTS"))
               ;; Mock completing-read
               ((symbol-function 'completing-read)
                (lambda (&rest _) "multi-cmd.txt"))
               ;; Mock read-string to provide different arguments
               ((symbol-function 'read-string)
                (let ((args '("first" "second" "third")))
                  (lambda (&rest _)
                    (pop args)))))
       
       (claude-code-emacs-execute-global-command)
       (should claude-code-emacs-send-string-called)
       (should (equal "/user:multi-cmd.txt first second third" 
                      claude-code-emacs-send-string-arg))))))

(provide 'test-claude-code-emacs)
;;; test-claude-code-emacs.el ends here
