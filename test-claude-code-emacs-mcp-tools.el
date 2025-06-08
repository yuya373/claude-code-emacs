;;; test-claude-code-emacs-mcp-tools.el --- Tests for MCP tool handlers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP tool handlers

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-tools)
(require 'cl-lib)

;;; Tool handler tests

(ert-deftest test-mcp-handle-openFile ()
  "Test openFile handler."
  (let* ((test-file (make-temp-file "test-file"))
         (params `((path . ,test-file)
                   (startText . "start")
                   (endText . "end"))))
    (unwind-protect
        (with-temp-buffer
          (insert "some start text end here")
          (write-file test-file)
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file))))
            (let ((result (claude-code-emacs-mcp-handle-openFile params)))
              (should (assoc 'success result))
              (should (equal (cdr (assoc 'success result)) t)))))
      (delete-file test-file))))

(ert-deftest test-mcp-handle-getOpenBuffers ()
  "Test getOpenBuffers handler."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/test/project/")))
    (let ((test-buffer (generate-new-buffer "test-file1.el")))
      (unwind-protect
          (with-current-buffer test-buffer
            (setq buffer-file-name "/test/project/file1.el")
            (let ((result (claude-code-emacs-mcp-handle-getOpenBuffers '((includeHidden . nil)))))
              (should (assoc 'buffers result))
              (let ((buffers (cdr (assoc 'buffers result))))
                (should (> (length buffers) 0))
                (let ((buffer-info (seq-find (lambda (b)
                                               (equal (cdr (assoc 'path b))
                                                      "/test/project/file1.el"))
                                             buffers)))
                  (should buffer-info)
                  (should (assoc 'path buffer-info))
                  (should (assoc 'name buffer-info))
                  (should (assoc 'active buffer-info))
                  (should (assoc 'modified buffer-info))))))
        (kill-buffer test-buffer)))))

(ert-deftest test-mcp-handle-getCurrentSelection ()
  "Test getCurrentSelection handler."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3")
    (goto-char (point-min))
    (push-mark (point-max) t t)
    (activate-mark)
    (let ((result (claude-code-emacs-mcp-handle-getCurrentSelection nil)))
      (should (assoc 'text result))
      (should (equal (cdr (assoc 'text result)) "Line 1\nLine 2\nLine 3"))
      (should (assoc 'startLine result))
      (should (assoc 'endLine result)))))

(ert-deftest test-mcp-handle-getDiagnostics ()
  "Test getDiagnostics handler."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (eq sym 'lsp-diagnostics)))
            ((symbol-function 'lsp-diagnostics)
             (lambda () (make-hash-table))))
    (let ((result (claude-code-emacs-mcp-handle-getDiagnostics nil)))
      (should (assoc 'diagnostics result))
      (should (listp (cdr (assoc 'diagnostics result)))))))

;;; Diff tool handler tests

(ert-deftest test-mcp-handle-openDiff ()
  "Test openDiff handler."
  (let* ((test-file-a (make-temp-file "test-diff-a"))
         (test-file-b (make-temp-file "test-diff-b"))
         (params `((mode . "files")
                   (fileA . ,(file-name-nondirectory test-file-a))
                   (fileB . ,(file-name-nondirectory test-file-b)))))
    (unwind-protect
        (progn
          (with-temp-file test-file-a
            (insert "Line 1\nLine 2\nLine 3\n"))
          (with-temp-file test-file-b
            (insert "Line 1\nLine 2 modified\nLine 3\n"))
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file-a)))
                    ((symbol-function 'ediff-files)
                     (lambda (file-a file-b)
                       (should (file-exists-p file-a))
                       (should (file-exists-p file-b)))))
            (let ((result (claude-code-emacs-mcp-handle-openDiff params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success")))))
      (delete-file test-file-a)
      (delete-file test-file-b))))

(ert-deftest test-mcp-handle-openDiff-buffers ()
  "Test openDiff handler with buffers."
  (let ((buffer-a (generate-new-buffer "*test-buffer-a*"))
        (buffer-b (generate-new-buffer "*test-buffer-b*")))
    (unwind-protect
        (progn
          (with-current-buffer buffer-a
            (insert "Buffer A content"))
          (with-current-buffer buffer-b
            (insert "Buffer B content"))
          (cl-letf (((symbol-function 'ediff-buffers)
                     (lambda (buf-a buf-b)
                       (should (get-buffer buf-a))
                       (should (get-buffer buf-b)))))
            (let* ((params `((mode . "buffers")
                            (bufferA . ,(buffer-name buffer-a))
                            (bufferB . ,(buffer-name buffer-b))))
                   (result (claude-code-emacs-mcp-handle-openDiff params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success")))))
      (kill-buffer buffer-a)
      (kill-buffer buffer-b))))

(ert-deftest test-mcp-handle-openDiff3 ()
  "Test openDiff3 handler."
  (let* ((test-file-a (make-temp-file "test-diff3-a"))
         (test-file-b (make-temp-file "test-diff3-b"))
         (test-file-c (make-temp-file "test-diff3-c"))
         (params `((fileA . ,(file-name-nondirectory test-file-a))
                   (fileB . ,(file-name-nondirectory test-file-b))
                   (fileC . ,(file-name-nondirectory test-file-c)))))
    (unwind-protect
        (progn
          (dolist (file (list test-file-a test-file-b test-file-c))
            (with-temp-file file
              (insert "Test content")))
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file-a)))
                    ((symbol-function 'ediff-files3)
                     (lambda (file-a file-b file-c)
                       (should (file-exists-p file-a))
                       (should (file-exists-p file-b))
                       (should (file-exists-p file-c)))))
            (let ((result (claude-code-emacs-mcp-handle-openDiff3 params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success")))))
      (delete-file test-file-a)
      (delete-file test-file-b)
      (delete-file test-file-c))))

(ert-deftest test-mcp-handle-openRevisionDiff ()
  "Test openRevisionDiff handler."
  (let* ((test-file (make-temp-file "test-revision"))
         (params `((file . ,(file-name-nondirectory test-file))
                   (revision . "HEAD"))))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Test content"))
          ;; Test case 1: File under version control (uses vc-version-ediff)
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file)))
                    ((symbol-function 'find-file-noselect)
                     (lambda (file &optional nowarn rawfile wildcards)
                       (with-current-buffer (get-buffer-create "*test-buffer*")
                         (setq buffer-file-name file)
                         (current-buffer))))
                    ((symbol-function 'vc-backend)
                     (lambda (file) 'git))  ;; Pretend file is under git
                    ;; Mock vc-version-ediff completely
                    ((symbol-function 'vc-version-ediff)
                     (lambda (files &optional rev1 rev2)
                       (should (listp files))
                       (should (equal rev1 "HEAD"))
                       (should (null rev2))
                       ;; Just message and return to avoid calling real VC
                       (message "Mock vc-version-ediff called")
                       t)))
            (let ((result (claude-code-emacs-mcp-handle-openRevisionDiff params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success"))))

          ;; Test case 2: File NOT under version control (should return error)
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file)))
                    ((symbol-function 'vc-backend)
                     (lambda (file) nil)))  ;; File not under version control
            (let ((result (claude-code-emacs-mcp-handle-openRevisionDiff params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "error"))
              (should (assoc 'message result))
              (should (string-match "not under version control" (cdr (assoc 'message result)))))))
      (delete-file test-file))))

(ert-deftest test-mcp-handle-openCurrentChanges ()
  "Test openCurrentChanges handler."
  (let* ((test-file (make-temp-file "test-changes"))
         (params `((file . ,(file-name-nondirectory test-file)))))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "Test content"))
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file)))
                    ((symbol-function 'vc-diff)
                     (lambda (&rest args) t)))
            (let ((result (claude-code-emacs-mcp-handle-openCurrentChanges params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success")))))
      (delete-file test-file))))

(ert-deftest test-mcp-handle-applyPatch ()
  "Test applyPatch handler."
  (let* ((patch-file (make-temp-file "test-patch" nil ".patch"))
         (target-file (make-temp-file "test-target"))
         (params `((patchFile . ,(file-name-nondirectory patch-file))
                   (targetFile . ,(file-name-nondirectory target-file)))))
    (unwind-protect
        (progn
          (with-temp-file patch-file
            (insert "--- a/file\n+++ b/file\n@@ -1 +1 @@\n-old\n+new\n"))
          (with-temp-file target-file
            (insert "old content"))
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory patch-file)))
                    ((symbol-function 'ediff-patch-file)
                     (lambda (patch target)
                       (should (file-exists-p patch))
                       (should (file-exists-p target)))))
            (let ((result (claude-code-emacs-mcp-handle-applyPatch params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success")))))
      (delete-file patch-file)
      (delete-file target-file))))

(ert-deftest test-mcp-handle-runCommand ()
  "Test runCommand handler."
  ;; Test successful command execution
  (let ((params '((command . "save-buffer"))))
    (cl-letf (((symbol-function 'save-buffer)
               (lambda () nil))
              ((symbol-function 'commandp)
               (lambda (sym) (eq sym 'save-buffer)))
              ((symbol-function 'buffer-modified-p)
               (lambda () nil)))
      (let ((result (claude-code-emacs-mcp-handle-runCommand params)))
        (should (assoc 'success result))
        (should (equal (cdr (assoc 'success result)) t)))))

  ;; Test command with arguments
  (let ((params '((command . "goto-line")
                  (args . (100)))))
    (cl-letf (((symbol-function 'goto-line)
               (lambda (line) line))
              ((symbol-function 'commandp)
               (lambda (sym) (memq sym '(save-buffer goto-line)))))
      (let ((result (claude-code-emacs-mcp-handle-runCommand params)))
        (should (assoc 'success result))
        (should (equal (cdr (assoc 'success result)) t))
        (should (equal (cdr (assoc 'result result)) 100)))))

  ;; Test unknown command
  (let ((params '((command . "unknown-command-xyz"))))
    (let ((result (claude-code-emacs-mcp-handle-runCommand params)))
      (should (assoc 'success result))
      (should (equal (cdr (assoc 'success result)) nil))
      (should (string-match "Unknown command" (cdr (assoc 'error result))))))

  ;; Test blocked command
  (let ((params '((command . "shell-command"))))
    (let ((result (claude-code-emacs-mcp-handle-runCommand params)))
      (should (assoc 'success result))
      (should (equal (cdr (assoc 'success result)) nil))
      (should (string-match "blocked for security" (cdr (assoc 'error result))))))

  ;; Test non-command symbol
  (let ((params '((command . "car"))))
    (let ((result (claude-code-emacs-mcp-handle-runCommand params)))
      (should (assoc 'success result))
      (should (equal (cdr (assoc 'success result)) nil))
      (should (string-match "Not a command" (cdr (assoc 'error result)))))))

(ert-deftest test-mcp-command-blocked-p ()
  "Test command blocking function."
  ;; Blocked commands
  (should (claude-code-emacs-mcp-command-blocked-p "shell-command"))
  (should (claude-code-emacs-mcp-command-blocked-p "eval-expression"))
  (should (claude-code-emacs-mcp-command-blocked-p "delete-file"))
  (should (claude-code-emacs-mcp-command-blocked-p "kill-emacs"))

  ;; Allowed commands
  (should-not (claude-code-emacs-mcp-command-blocked-p "save-buffer"))
  (should-not (claude-code-emacs-mcp-command-blocked-p "goto-line"))
  (should-not (claude-code-emacs-mcp-command-blocked-p "indent-region")))

(ert-deftest test-mcp-serialize-value ()
  "Test value serialization."
  ;; nil -> :null
  (should (eq (claude-code-emacs-mcp-serialize-value nil) :null))

  ;; Simple types
  (should (equal (claude-code-emacs-mcp-serialize-value 42) 42))
  (should (equal (claude-code-emacs-mcp-serialize-value "hello") "hello"))
  (should (equal (claude-code-emacs-mcp-serialize-value t) t))

  ;; Symbol -> string
  (should (equal (claude-code-emacs-mcp-serialize-value 'foo) "foo"))

  ;; List -> array
  (should (equal (claude-code-emacs-mcp-serialize-value '(1 2 3)) '(1 2 3)))

  ;; Buffer -> buffer info
  (with-temp-buffer
    (let ((result (claude-code-emacs-mcp-serialize-value (current-buffer))))
      (should (assoc 'type result))
      (should (equal (cdr (assoc 'type result)) "buffer"))
      (should (assoc 'name result)))))

;;; Tests for definition finding

(ert-deftest test-mcp-handle-getDefinition-with-lsp ()
  "Test getting definition with LSP."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun test-func () nil)")
    (goto-char (point-min))
    (search-forward "test-func")
    (cl-letf* ((test-file (make-temp-file "test-def" nil ".el"))
               ((symbol-function 'fboundp)
                (lambda (sym) (memq sym '(lsp-mode lsp-request))))
               ((symbol-function 'bound-and-true-p)
                (lambda (sym) (eq sym 'lsp-mode)))
               ((symbol-value 'lsp-mode) t)
               ((symbol-function 'lsp--text-document-position-params)
                (lambda () '(:textDocument (:uri "file:///test.el") :position (:line 0 :character 7))))
               ((symbol-function 'lsp-request)
                (lambda (method params)
                  (when (string= method "textDocument/definition")
                    `(:uri ,(concat "file://" test-file) :range (:start (:line 0 :character 0) :end (:line 0 :character 23))))))
               ((symbol-function 'lsp--uri-to-path)
                (lambda (uri) test-file))
               ((symbol-function 'claude-code-emacs-mcp-get-definition-info-at)
                (lambda (file line column)
                  `((file . ,file)
                    (line . ,line)
                    (column . ,column)
                    (symbol . "test-func")
                    (type . "function")
                    (preview . "(defun test-func () nil)")))))
      (unwind-protect
          (progn
            (with-temp-file test-file
              (insert "(defun test-func () nil)"))
            (let ((result (claude-code-emacs-mcp-handle-getDefinition
                           `((symbol . "test-func")
                             (file . ,(file-name-nondirectory test-file))))))
              (should (listp (cdr (assoc 'definitions result))))
              (should (string= (cdr (assoc 'method result)) "lsp"))
              (should (string= (cdr (assoc 'searchedSymbol result)) "test-func"))))
        (delete-file test-file)))))


(ert-deftest test-mcp-handle-getDefinition-no-results ()
  "Test getting definition with no results."
  (cl-letf (((symbol-function 'fboundp) (lambda (_) nil)))
    (should-error
     (claude-code-emacs-mcp-handle-getDefinition
      '((symbol . "non-existent-func")
        (file . "test.el")))
     :type 'error)))

(ert-deftest test-mcp-handle-getDefinition-missing-file ()
  "Test getting definition with missing file parameter."
  (should-error
   (claude-code-emacs-mcp-handle-getDefinition
    '((symbol . "test-func")))
   :type 'error))

(ert-deftest test-mcp-capture-definition-at-point ()
  "Test capturing definition information at point."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-test-function (arg)\n  \"Doc string.\"\n  (message \"Hello %s\" arg))\n")
    (goto-char (point-min))
    (search-forward "my-test-function")
    (let ((result (claude-code-emacs-mcp-capture-definition-at-point)))
      (should (assoc 'line result))
      (should (= (cdr (assoc 'line result)) 1))
      (should (assoc 'column result))
      (should (assoc 'symbol result))
      (should (string= (cdr (assoc 'symbol result)) "my-test-function"))
      (should (assoc 'type result))
      (should (string= (cdr (assoc 'type result)) "function"))
      (should (assoc 'preview result))
      (should (string-match "defun my-test-function" (cdr (assoc 'preview result)))))))

(ert-deftest test-mcp-guess-definition-type ()
  "Test guessing definition types."
  (with-temp-buffer
    (emacs-lisp-mode)
    ;; Test function
    (insert "(defun test-func () nil)\n")
    (goto-char (point-min))
    (should (string= (claude-code-emacs-mcp-guess-definition-type) "function"))

    ;; Test variable
    (erase-buffer)
    (insert "(defvar test-var 'value)\n")
    (goto-char (point-min))
    (should (string= (claude-code-emacs-mcp-guess-definition-type) "variable"))

    ;; Test macro
    (erase-buffer)
    (insert "(defmacro test-macro () nil)\n")
    (goto-char (point-min))
    (should (string= (claude-code-emacs-mcp-guess-definition-type) "macro"))

    ;; Test class
    (erase-buffer)
    (insert "(defclass test-class () ())\n")
    (goto-char (point-min))
    (should (string= (claude-code-emacs-mcp-guess-definition-type) "class"))))

(ert-deftest test-mcp-handle-get-buffer-content ()
  "Test getting buffer content."
  (let* ((test-file (make-temp-file "test-file"))
         (test-buffer (generate-new-buffer "test-buffer")))
    (unwind-protect
        (progn
          ;; Write content to actual file
          (with-temp-file test-file
            (insert "Line 1\nLine 2\nLine 3\n"))
          ;; Visit the file in a buffer
          (with-current-buffer test-buffer
            (insert "Line 1\nLine 2\nLine 3\n")
            (set-visited-file-name test-file))
          ;; Test getting content
          (cl-letf (((symbol-function 'projectile-project-root)
                     (lambda () (file-name-directory test-file)))
                    ((symbol-function 'claude-code-emacs-normalize-project-root)
                     (lambda (root) root)))
            (let ((result (claude-code-emacs-mcp-handle-get-buffer-content
                           `((path . ,(file-name-nondirectory test-file))))))
              (should (equal (cdr (assoc 'success result)) t))
              (should (equal (cdr (assoc 'content result))
                            "Line 1\nLine 2\nLine 3\n")))))
      (kill-buffer test-buffer)
      (delete-file test-file))))

(ert-deftest test-mcp-handle-get-project-info ()
  "Test getting project info."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/test/project/"))
            ((symbol-function 'projectile-project-name)
             (lambda () "test-project"))
            ((symbol-function 'projectile-project-type)
             (lambda () 'generic))
            ((symbol-function 'claude-code-emacs-normalize-project-root)
             (lambda (root) root))
            ((symbol-function 'vc-responsible-backend)
             (lambda (_) nil)))
    (let ((result (claude-code-emacs-mcp-handle-get-project-info nil)))
      (should (equal (cdr (assoc 'success result)) t))
      (should (equal (cdr (assoc 'projectRoot result)) "/test/project/"))
      (should (equal (cdr (assoc 'projectName result)) "test-project")))))

(ert-deftest test-mcp-handle-get-project-files ()
  "Test getting project files."
  (cl-letf (((symbol-function 'projectile-project-root)
             (lambda () "/test/project/"))
            ((symbol-function 'projectile-current-project-files)
             (lambda () '("file1.el" "src/file2.el" "test/file3.el")))
            ((symbol-function 'claude-code-emacs-normalize-project-root)
             (lambda (root) root)))
    (let ((result (claude-code-emacs-mcp-handle-get-project-files nil)))
      (should (equal (cdr (assoc 'success result)) t))
      (let ((files (cdr (assoc 'files result))))
        (should (= (length files) 3))
        ;; Check that each file has the expected structure
        (let ((paths (mapcar (lambda (f) (cdr (assoc 'path f))) files)))
          (should (member "file1.el" paths))
          (should (member "src/file2.el" paths))
          (should (member "test/file3.el" paths)))))))

(ert-deftest test-mcp-get-lsp-definitions-with-request ()
  "Test getting LSP definitions with request."
  (cl-letf* ((lsp-response nil)
             ((symbol-function 'lsp--text-document-position-params)
              (lambda () '(:textDocument (:uri "file:///test.el") :position (:line 0 :character 7))))
             ((symbol-function 'lsp-request)
              (lambda (method params)
                (when (string= method "textDocument/definition")
                  lsp-response)))
             ((symbol-function 'lsp--uri-to-path)
              (lambda (uri) (substring uri 7)))  ;; Remove "file://" prefix
             ((symbol-function 'claude-code-emacs-mcp-get-definition-info-at)
              (lambda (file line column)
                `((file . ,file)
                  (line . ,line)
                  (column . ,column)
                  (symbol . "test-symbol")
                  (type . "function")
                  (preview . "function test-symbol() {}")))))
    ;; Test single location response
    (setq lsp-response (list :uri "file:///test1.el" :range (list :start (list :line 5 :character 10))))
    (let ((result (claude-code-emacs-mcp-get-lsp-definitions-with-request)))
      (should (= (length result) 1))
      (let ((def (car result)))
        (should (equal (cdr (assoc 'file def)) "/test1.el"))
        (should (equal (cdr (assoc 'line def)) 6))  ;; 1-based
        (should (equal (cdr (assoc 'column def)) 10))))
    
    ;; Test multiple locations response  
    (setq lsp-response (list (list :uri "file:///test1.el" :range (list :start (list :line 5 :character 10)))
                            (list :uri "file:///test2.el" :range (list :start (list :line 10 :character 5)))))
    (let ((result (claude-code-emacs-mcp-get-lsp-definitions-with-request)))
      (should (= (length result) 2)))
    
    ;; Test LocationLink response
    (setq lsp-response (list :targetUri "file:///test3.el" :targetRange (list :start (list :line 15 :character 20))))
    (let ((result (claude-code-emacs-mcp-get-lsp-definitions-with-request)))
      (should (= (length result) 1))
      (let ((def (car result)))
        (should (equal (cdr (assoc 'file def)) "/test3.el"))
        (should (equal (cdr (assoc 'line def)) 16))))))  ;; 1-based

(ert-deftest test-mcp-get-definition-info-at ()
  "Test getting definition info at specific location."
  (let ((test-file (make-temp-file "test-def" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-function (arg)\n")
            (insert "  \"Documentation string.\"\n")
            (insert "  (message \"Hello %s\" arg))\n"))
          (let ((result (claude-code-emacs-mcp-get-definition-info-at test-file 1 7)))
            (should result)
            (should (equal (cdr (assoc 'file result)) test-file))
            (should (equal (cdr (assoc 'line result)) 1))
            (should (equal (cdr (assoc 'column result)) 7))
            (should (equal (cdr (assoc 'symbol result)) "test-function"))
            (should (equal (cdr (assoc 'type result)) "function"))
            (should (string-match "defun test-function" (cdr (assoc 'preview result))))))
      (delete-file test-file))))

(ert-deftest test-mcp-get-definition-preview ()
  "Test getting definition preview."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun my-long-function (arg1 arg2 arg3)\n")
    (insert "  \"This is a very long documentation string that goes on and on.\n")
    (insert "  It has multiple lines and lots of detail about what the function does.\n")
    (insert "  We want to make sure the preview is limited to a reasonable size.\"\n")
    (insert "  (let ((result nil))\n")
    (insert "    (dolist (item arg1)\n")
    (insert "      (when (member item arg2)\n")
    (insert "        (push item result)))\n")
    (insert "    (append result arg3)))\n")
    (goto-char (point-min))
    (forward-char 7)  ;; Position at function name
    (let ((preview (claude-code-emacs-mcp-get-definition-preview)))
      (should preview)
      (should (string-match "defun my-long-function" preview))
      ;; Check that preview is limited in size
      (should (<= (length preview) 500)))))

(provide 'test-claude-code-emacs-mcp-tools)
;;; test-claude-code-emacs-mcp-tools.el ends here
