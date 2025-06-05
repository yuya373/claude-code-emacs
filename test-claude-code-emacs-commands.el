;;; test-claude-code-emacs-commands.el --- Tests for slash commands and custom commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs-commands module

;;; Code:

(require 'ert)
(require 'claude-code-emacs-commands)
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

;;; Tests for slash commands

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

    ;; Test config command (now simplified to just send /config)
    (claude-code-emacs-config)
    (should (member "/config" sent-commands))

    (claude-code-emacs-compact "")
    (should (member "/compact" sent-commands))

    (claude-code-emacs-compact "focus on tests")
    (should (member "/compact focus on tests" sent-commands))))

(ert-deftest test-claude-code-emacs-key-sending-commands ()
  "Test key sending commands."
  (with-claude-mock-buffer
   (let ((keys-sent nil))
     ;; Mock vterm functions
     (cl-letf (((symbol-function 'vterm-send-escape)
                (lambda () (push 'escape keys-sent)))
               ((symbol-function 'vterm-send-return)
                (lambda () (push 'return keys-sent)))
               ((symbol-function 'vterm-send-key)
                (lambda (key &optional shift) (push (list 'key key 'shift shift) keys-sent)))
               ((symbol-function 'kbd)
                (lambda (key-string) key-string)))

       (claude-code-emacs-send-escape)
       (should (member 'escape keys-sent))

       (claude-code-emacs-send-return)
       (should (member 'return keys-sent))

       (claude-code-emacs-send-ctrl-r)
       ;; kbd actually returns "\C-r" (ASCII 22)
       (should (member '(key "\C-r" shift nil) keys-sent))

       ;; Test shift-tab sending
       (claude-code-emacs-send-shift-tab)
       (should (member '(key "<tab>" shift t) keys-sent))))))

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
  ;; We'll just test that the function doesn't error and returns a list
  (let ((result (claude-code-emacs-list-global-command-files)))
    (should (or (null result) (listp result)))
    ;; If there are results, they should all end with .md
    (when result
      (dolist (file result)
        (should (string-suffix-p ".md" file))))))

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
                (lambda () '("test-command.md" "another-command.md")))
               ;; Mock read-global-command-file to return content without $ARGUMENTS
               ((symbol-function 'claude-code-emacs-read-global-command-file)
                (lambda (file) "Simple command content"))
               ;; Mock completing-read to select a file
               ((symbol-function 'completing-read)
                (lambda (&rest _) "test-command.md")))

       (claude-code-emacs-execute-global-command)
       (should claude-code-emacs-send-string-called)
       (should (equal "/user:test-command" claude-code-emacs-send-string-arg))))))

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
                (lambda () '("multi-cmd.md")))
               ;; Mock read-global-command-file
               ((symbol-function 'claude-code-emacs-read-global-command-file)
                (lambda (file) "Do $ARGUMENTS with $ARGUMENTS and $ARGUMENTS"))
               ;; Mock completing-read
               ((symbol-function 'completing-read)
                (lambda (&rest _) "multi-cmd.md"))
               ;; Mock read-string to provide different arguments
               ((symbol-function 'read-string)
                (let ((args '("first" "second" "third")))
                  (lambda (&rest _)
                    (pop args)))))

       (claude-code-emacs-execute-global-command)
       (should claude-code-emacs-send-string-called)
       (should (equal "/user:multi-cmd first second third"
                      claude-code-emacs-send-string-arg))))))

(provide 'test-claude-code-emacs-commands)
;;; test-claude-code-emacs-commands.el ends here
