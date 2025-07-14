;;; test-claude-code-commands.el --- Tests for slash commands and custom commands -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-commands module

;;; Code:

(require 'ert)
(require 'claude-code-commands)
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

;;; Tests for slash commands

(ert-deftest test-claude-code-slash-commands ()
  "Test slash command functions."
  (cl-letf* ((sent-commands nil)
             ((symbol-function 'claude-code-send-string)
              (lambda (str &optional paste-p)
                (push str sent-commands))))

    ;; Test simple commands
    (claude-code-init)
    (should (member "/init" sent-commands))

    (claude-code-clear)
    (should (member "/clear" sent-commands))

    (claude-code-help)
    (should (member "/help" sent-commands))

    ;; Test config command (now simplified to just send /config)
    (claude-code-config)
    (should (member "/config" sent-commands))

    (claude-code-compact "")
    (should (member "/compact" sent-commands))

    (claude-code-compact "focus on tests")
    (should (member "/compact focus on tests" sent-commands))))

(ert-deftest test-claude-code-key-sending-commands ()
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

       (claude-code-send-escape)
       (should (member 'escape keys-sent))

       (claude-code-send-return)
       (should (member 'return keys-sent))

       (claude-code-send-ctrl-r)
       ;; kbd actually returns "\C-r" (ASCII 22)
       (should (member '(key "\C-r" shift nil) keys-sent))

       ;; Test shift-tab sending
       (claude-code-send-shift-tab)
       (should (member '(key "<tab>" shift t) keys-sent))))))

;;; Tests for custom project command functions

(ert-deftest test-claude-code-custom-commands-directory ()
  "Test custom commands directory path generation."
  (with-claude-test-project
   (let ((commands-dir (claude-code-custom-commands-directory)))
     (should (string-match-p "\\.claude/commands$" commands-dir))
     (should (string-prefix-p temp-dir commands-dir)))))

(ert-deftest test-claude-code-list-custom-command-files ()
  "Test listing custom command files."
  (with-claude-test-project
   ;; Test when directory doesn't exist
   (should (null (claude-code-list-custom-command-files)))

   ;; Create commands directory and files
   (let ((commands-dir (claude-code-custom-commands-directory)))
     (make-directory commands-dir t)

     ;; Create some test command files
     (with-temp-file (expand-file-name "test1.md" commands-dir)
       (insert "test command 1"))
     (with-temp-file (expand-file-name "test2.md" commands-dir)
       (insert "test command 2"))
     (with-temp-file (expand-file-name "not-markdown.txt" commands-dir)
       (insert "not a command"))

     ;; Test listing
     (let ((files (claude-code-list-custom-command-files)))
       (should (= 2 (length files)))
       (should (member "test1.md" files))
       (should (member "test2.md" files))
       (should-not (member "not-markdown.txt" files))))))

(ert-deftest test-claude-code-read-custom-command-file ()
  "Test reading custom command file contents."
  (with-claude-test-project
   (let ((commands-dir (claude-code-custom-commands-directory)))
     (make-directory commands-dir t)

     ;; Create test file
     (with-temp-file (expand-file-name "test-command.md" commands-dir)
       (insert "  test command content  \n"))

     ;; Test reading
     (should (equal "test command content"
                    (claude-code-read-custom-command-file "test-command.md")))

     ;; Test non-existent file
     (should (null (claude-code-read-custom-command-file "non-existent.md"))))))


;;; Tests for global command functions

(ert-deftest test-claude-code-global-commands-directory ()
  "Test global commands directory path generation."
  (let ((commands-dir (claude-code-global-commands-directory)))
    (should (string-match-p "/.claude/commands$" commands-dir))
    (should (string-prefix-p (expand-file-name "~") commands-dir))))

(ert-deftest test-claude-code-list-global-command-files ()
  "Test listing global command files."
  ;; This test is limited because we can't easily mock the home directory
  ;; We'll just test that the function doesn't error and returns a list
  (let ((result (claude-code-list-global-command-files)))
    (should (or (null result) (listp result)))
    ;; If there are results, they should all end with .md
    (when result
      (dolist (file result)
        (should (string-suffix-p ".md" file))))))

(ert-deftest test-claude-code-read-global-command-file ()
  "Test reading global command file contents."
  ;; This test is limited because we can't easily mock the home directory
  ;; We'll just test that the function doesn't error with non-existent file
  (should (null (claude-code-read-global-command-file "non-existent.txt"))))


(ert-deftest test-claude-code-execute-custom-command-with-multiple-args ()
  "Test custom command execution with multiple $ARGUMENTS."
  (with-claude-mock-buffer
   (let ((commands-dir (claude-code-custom-commands-directory))
         (claude-code-send-string-called nil)
         (claude-code-send-string-arg nil))
     ;; Mock send-string to capture calls
     (cl-letf (((symbol-function 'claude-code-send-string)
                (lambda (str)
                  (setq claude-code-send-string-called t
                        claude-code-send-string-arg str))))

       ;; Create commands directory and test file
       (make-directory commands-dir t)
       (with-temp-file (expand-file-name "multi-arg.md" commands-dir)
         (insert "Command with $ARGUMENTS and $ARGUMENTS"))

       ;; Mock user input
       (cl-letf (((symbol-function 'completing-read)
                  (lambda (&rest _) "project:multi-arg"))
                 ((symbol-function 'read-string)
                  (let ((counter 0))
                    (lambda (&rest _)
                      (setq counter (1+ counter))
                      (format "arg%d" counter)))))

         (claude-code-execute-custom-command)
         (should claude-code-send-string-called)
         (should (equal "/multi-arg arg1 arg2" claude-code-send-string-arg)))))))


;;; Tests for unified command execution

(ert-deftest test-claude-code-get-custom-commands ()
  "Test getting all custom commands with prefixes."
  (with-claude-test-project
   (let ((project-commands-dir (claude-code-custom-commands-directory))
         (user-commands-dir (claude-code-global-commands-directory)))

     ;; Create project commands
     (make-directory project-commands-dir t)
     (with-temp-file (expand-file-name "project-cmd1.md" project-commands-dir)
       (insert "Project command 1"))
     (with-temp-file (expand-file-name "project-cmd2.md" project-commands-dir)
       (insert "Project command 2"))

     ;; Mock global commands
     (cl-letf (((symbol-function 'claude-code-list-global-command-files)
                (lambda () '("user-cmd1.md" "user-cmd2.md"))))

       (let ((commands (claude-code-get-custom-commands)))
         ;; Check we have all commands
         (should (= 4 (length commands)))

         ;; Check project commands
         (should (assoc "project:project-cmd1" commands))
         (should (assoc "project:project-cmd2" commands))

         ;; Check user commands
         (should (assoc "user:user-cmd1" commands))
         (should (assoc "user:user-cmd2" commands))

         ;; Check command info structure
         (let ((project-info (cdr (assoc "project:project-cmd1" commands))))
           (should (eq 'project (cdr (assoc 'type project-info))))
           (should (equal "project-cmd1.md" (cdr (assoc 'filename project-info)))))

         (let ((user-info (cdr (assoc "user:user-cmd1" commands))))
           (should (eq 'user (cdr (assoc 'type user-info))))
           (should (equal "user-cmd1.md" (cdr (assoc 'filename user-info))))))))))

(ert-deftest test-claude-code-fix-diagnostic ()
  "Test fixing diagnostics using lsp-diagnostics."
  (with-claude-mock-buffer
   (let ((claude-code-send-string-called nil)
         (claude-code-send-string-arg nil)
         (test-diagnostics (make-hash-table :test 'equal)))

     ;; Mock functions
     (cl-letf (((symbol-function 'claude-code-send-string)
                (lambda (str)
                  (setq claude-code-send-string-called t
                        claude-code-send-string-arg str)))
               ((symbol-function 'lsp-mode) nil)
               ((symbol-value 'lsp-mode) t)
               ((symbol-function 'buffer-file-name) 
                (lambda () "/home/user/project/src/main.el"))
               ((symbol-function 'projectile-project-root)
                (lambda () "/home/user/project"))
               ((symbol-function 'file-relative-name)
                (lambda (file root) "src/main.el"))
               ((symbol-function 'completing-read)
                (lambda (prompt collection &rest _)
                  "[ERROR] src/main.el:10 - Undefined variable 'foo'"))
               ((symbol-function 'lsp-diagnostics)
                (lambda ()
                  ;; Create test diagnostics using lsp-interface structures
                  (require 'lsp-protocol)
                  (puthash "/home/user/project/src/main.el"
                           (list (lsp-make-diagnostic
                                  :severity? 1
                                  :message "Undefined variable 'foo'"
                                  :range (lsp-make-range
                                          :start (lsp-make-position :line 9 :character 0)
                                          :end (lsp-make-position :line 9 :character 10))))
                           test-diagnostics)
                  test-diagnostics)))

       ;; Call the function
       (claude-code-fix-diagnostic)
       
       ;; Check the result
       (should claude-code-send-string-called)
       (should (string-match-p "Fix the following error in @src/main\\.el at line 10:" 
                               claude-code-send-string-arg))
       (should (string-match-p "Undefined variable 'foo'" 
                               claude-code-send-string-arg))
       (should (string-match-p "Please fix this issue\\." 
                               claude-code-send-string-arg))))))

(ert-deftest test-claude-code-fix-diagnostic-no-lsp ()
  "Test fix-diagnostic when LSP is not active."
  (with-claude-mock-buffer
   (cl-letf (((symbol-function 'lsp-mode) nil)
             ((symbol-value 'lsp-mode) nil))
     (should-error (claude-code-fix-diagnostic) :type 'user-error))))

(ert-deftest test-claude-code-fix-diagnostic-no-diagnostics ()
  "Test fix-diagnostic when no diagnostics are found."
  (with-claude-mock-buffer
   (let ((message-called nil)
         (message-arg nil))
     (cl-letf (((symbol-function 'message)
                (lambda (fmt &rest args)
                  (setq message-called t
                        message-arg (apply #'format fmt args))))
               ((symbol-function 'lsp-mode) nil)
               ((symbol-value 'lsp-mode) t)
               ((symbol-function 'lsp-diagnostics)
                (lambda () (make-hash-table :test 'equal))))
       
       (claude-code-fix-diagnostic)
       (should message-called)
       (should (equal "No diagnostics found" message-arg))))))

(ert-deftest test-claude-code-execute-custom-command-unified ()
  "Test custom command execution with both project and user commands."
  (with-claude-mock-buffer
   (let ((project-commands-dir (claude-code-custom-commands-directory))
         (user-commands-dir (expand-file-name ".claude/commands" (expand-file-name "~")))
         (claude-code-send-string-called nil)
         (claude-code-send-string-arg nil))

     ;; Create project command
     (make-directory project-commands-dir t)
     (with-temp-file (expand-file-name "test-project.md" project-commands-dir)
       (insert "project command content"))

     ;; Create user command directory
     (make-directory user-commands-dir t)

     ;; Mock functions
     (cl-letf (((symbol-function 'claude-code-send-string)
                (lambda (str)
                  (setq claude-code-send-string-called t
                        claude-code-send-string-arg str)))
               ((symbol-function 'read-string)
                (lambda (&rest _) "test-arg"))
               ((symbol-function 'completing-read)
                (let ((call-count 0))
                  (lambda (prompt collection &rest _)
                    (setq call-count (1+ call-count))
                    ;; First call returns project command, second returns user command
                    (if (= call-count 1)
                        "project:test-project"
                      "user:commit-push"))))
               ;; Mock the global command files
               ((symbol-function 'claude-code-list-global-command-files)
                (lambda () '("commit-push.md")))
               ;; Mock reading command file - mock the general function used by execute-custom-command
               ((symbol-function 'claude-code-read-command-file)
                (lambda (filepath) 
                  (cond
                   ((string-match-p "test-project\\.md$" filepath) "project command content")
                   ((string-match-p "commit-push\\.md$" filepath) "user command content")
                   (t nil)))))

       ;; Test project command
       (claude-code-execute-custom-command)
       (should claude-code-send-string-called)
       (should (equal "/test-project" claude-code-send-string-arg))

       ;; Reset and test user command
       (setq claude-code-send-string-called nil
             claude-code-send-string-arg nil)

       (claude-code-execute-custom-command)
       (should claude-code-send-string-called)
       ;; Commands are sent without prefix
       (should (equal "/commit-push" claude-code-send-string-arg))))))

(provide 'test-claude-code-commands)
;;; test-claude-code-commands.el ends here
