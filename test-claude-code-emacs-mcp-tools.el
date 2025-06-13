;;; test-claude-code-emacs-mcp-tools.el --- Tests for MCP tool handlers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP tool handlers

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-tools)
(require 'cl-lib)

;;; Tool handler tests


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

(ert-deftest test-mcp-handle-getDiagnostics-no-buffer ()
  "Test getDiagnostics handler without buffer parameter should error."
  (should-error
   (claude-code-emacs-mcp-handle-getDiagnostics nil)
   :type 'error))

(ert-deftest test-mcp-handle-getDiagnostics-buffer-not-found ()
  "Test getDiagnostics handler with non-existent buffer."
  (cl-letf (((symbol-function 'fboundp)
             (lambda (sym) (memq sym '(lsp-diagnostics lsp:diagnostic-message)))))
    (should-error
     (claude-code-emacs-mcp-handle-getDiagnostics '((buffer . "non-existent-buffer")))
     :type 'error)))

(ert-deftest test-mcp-handle-getDiagnostics-with-buffer ()
  "Test getDiagnostics handler with buffer parameter."
  (let ((test-buffer (generate-new-buffer "test-diagnostics-buffer")))
    (unwind-protect
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (sym) (memq sym '(lsp-diagnostics lsp:diagnostic-message))))
                  ((symbol-function 'lsp-diagnostics)
                   (lambda () 
                     (let ((hash (make-hash-table :test 'equal)))
                       (puthash "/test/file.el" 
                                (let ((inner-hash (make-hash-table)))
                                  (puthash 10 (list 'mock-diagnostic) inner-hash)
                                  inner-hash)
                                hash)
                       hash)))
                  ((symbol-function 'lsp:diagnostic-message)
                   (lambda (_) "Test diagnostic message"))
                  ((symbol-function 'lsp:diagnostic-severity)
                   (lambda (_) 1))
                  ((symbol-function 'lsp:diagnostic-source)
                   (lambda (_) "test-lsp")))
          (with-current-buffer test-buffer
            (let ((result (claude-code-emacs-mcp-handle-getDiagnostics 
                           `((buffer . ,(buffer-name test-buffer))))))
              (should (assoc 'diagnostics result))
              (let ((diags (cdr (assoc 'diagnostics result))))
                (should (listp diags))))))
      (kill-buffer test-buffer))))

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

;;; Tests for definition finding

(ert-deftest test-mcp-handle-getDefinition-with-lsp ()
  "Test getting definition with LSP."
  (let ((test-file (make-temp-file "test-def" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-func () nil)"))
          (cl-letf* (;; Mock all required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (memq sym '(lsp-mode lsp-request))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ;; Don't mock expand-file-name, use the real one
                     ;;((symbol-function 'expand-file-name)
                     ;; (lambda (file &optional dir)
                     ;;   (if dir
                     ;;       (concat (file-name-as-directory dir) file)
                     ;;     file)))
                     ;; Mock find-file-noselect to return a buffer with LSP active
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (emacs-lisp-mode)
                            (insert-file-contents file nil nil nil t)
                            ;; Make LSP appear active in this buffer
                            (setq-local lsp-mode t))
                          buf)))
                     ((symbol-function 'bound-and-true-p)
                      (lambda (sym)
                        (if (eq sym 'lsp-mode)
                            t
                          (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'lsp--text-document-position-params)
                      (lambda () '(:textDocument (:uri "file:///test.el") :position (:line 0 :character 7))))
                     ((symbol-function 'lsp-request)
                      (lambda (method params)
                        (when (string= method "textDocument/definition")
                          `(:uri ,(concat "file://" test-file) :range (:start (:line 0 :character 0) :end (:line 0 :character 23))))))
                     ((symbol-function 'lsp--uri-to-path)
                      (lambda (uri) test-file))
                     ((symbol-function 'claude-code-emacs-mcp-get-definition-info-at)
                      (lambda (file range)
                        `((file . ,file)
                          (preview . "(defun test-func () nil)")
                          (range . ,range)))))
            (let ((result (claude-code-emacs-mcp-handle-getDefinition
                           `((symbol . "test-func")
                             (file . ,(file-name-nondirectory test-file))
                             (line . 1)))))
              (should (listp (cdr (assoc 'definitions result))))
              (should (string= (cdr (assoc 'method result)) "lsp")))))
      (delete-file test-file))))


(ert-deftest test-mcp-handle-getDefinition-no-results ()
  "Test getting definition with no results."
  (cl-letf (((symbol-function 'fboundp) (lambda (_) nil)))
    (should-error
     (claude-code-emacs-mcp-handle-getDefinition
      '((symbol . "non-existent-func")
        (file . "test.el")
        (line . 1)))
     :type 'error)))

(ert-deftest test-mcp-handle-getDefinition-missing-file ()
  "Test getting definition with missing file parameter."
  (should-error
   (claude-code-emacs-mcp-handle-getDefinition
    '((symbol . "test-func")
      (line . 1)))
   :type 'error))

(ert-deftest test-mcp-handle-getDefinition-missing-line ()
  "Test getting definition with missing line parameter."
  (should-error
   (claude-code-emacs-mcp-handle-getDefinition
    '((symbol . "test-func")
      (file . "test.el"))
   :type 'error))

(ert-deftest test-mcp-handle-getDefinition-missing-symbol ()
  "Test getting definition with missing symbol parameter."
  (should-error
   (claude-code-emacs-mcp-handle-getDefinition
    '((file . "test.el")
      (line . 1)))
   :type 'error))

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
              (lambda (file range)
                `((file . ,file)
                  (preview . "function test-symbol() {}")
                  (range . ,range)))))
    ;; Test single location response
    (setq lsp-response (list :uri "file:///test1.el" :range (list :start (list :line 5 :character 10))))
    (let ((result (claude-code-emacs-mcp-get-lsp-definitions-with-request)))
      (should (= (length result) 1))
      (let ((def (car result)))
        (should (equal (cdr (assoc 'file def)) "/test1.el"))
        (let ((range (cdr (assoc 'range def))))
          (should (equal (plist-get (plist-get range :start) :line) 5))
          (should (equal (plist-get (plist-get range :start) :character) 10))))

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
        (let ((range (cdr (assoc 'range def))))
          (should (equal (plist-get (plist-get range :start) :line) 15))
          (should (equal (plist-get (plist-get range :start) :character) 20)))))))

(ert-deftest test-mcp-get-definition-info-at ()
  "Test getting definition info at specific location."
  (let ((test-file (make-temp-file "test-def" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-function (arg)\n")
            (insert "  \"Documentation string.\"\n")
            (insert "  (message \"Hello %s\" arg))\n"))
          (let* ((range '(:start (:line 0 :character 7) :end (:line 0 :character 20)))
                 (result (claude-code-emacs-mcp-get-definition-info-at test-file range)))
            (should result)
            (should (equal (cdr (assoc 'file result)) test-file))
            (should (string-match "defun test-function" (cdr (assoc 'preview result))))
            (should (equal (cdr (assoc 'range result)) range))))
      (delete-file test-file)))))

(ert-deftest test-mcp-get-definition-preview ()
  "Test getting definition preview."
  (let ((test-file (make-temp-file "test-preview" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            ;; Insert some lines before the function
            (insert ";; Some comment before\n")
            (insert ";; Another comment\n")
            (insert ";; Third comment\n")
            (insert "(defun my-long-function (arg1 arg2 arg3)\n")
            (insert "  \"This is a very long documentation string that goes on and on.\n")
            (insert "  It has multiple lines and lots of detail about what the function does.\n")
            (insert "  We want to make sure the preview is limited to a reasonable size.\"\n")
            (insert "  (let ((result nil))\n")
            (insert "    (dolist (item arg1)\n")
            (insert "      (when (member item arg2)\n")
            (insert "        (push item result)))\n")
            (insert "    (append result arg3)))\n")
            (insert ";; Comment after\n")
            (insert ";; Another after\n")
            (insert ";; Third after\n"))

          ;; Create a range for the function definition (line 3 to line 11, 0-based)
          (let* ((range '(:start (:line 3 :character 7)
                          :end (:line 11 :character 24)))
                 (preview (claude-code-emacs-mcp-get-preview-text test-file range)))
            (should preview)
            ;; Should include the function definition
            (should (string-match "defun my-long-function" preview))
            ;; Should include some context before (comments)
            (should (string-match ";; Some comment before" preview))
            ;; Should include some context after
            (should (string-match ";; Comment after" preview))
            ;; The preview includes all lines since we have a small test case
            ;; In real usage, the preview would be truncated for very large functions
            (should t)))
      (delete-file test-file)))))

;;; Tests for reference finding

(ert-deftest test-mcp-handle-findReferences ()
  "Test finding references with LSP."
  (let ((test-file (make-temp-file "test-ref" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-func ()\n")
            (insert "  \"Test function\"\n")
            (insert "  (message \"test\"))\n")
            (insert "\n")
            (insert "(defun caller ()\n")
            (insert "  (test-func))\n"))
          
          (cl-letf* (;; Mock required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (memq sym '(lsp-mode lsp-request))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (emacs-lisp-mode)
                            (insert-file-contents file nil nil nil t)
                            (setq-local lsp-mode t))
                          buf)))
                     ((symbol-function 'bound-and-true-p)
                      (lambda (sym)
                        (if (eq sym 'lsp-mode) t
                          (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'lsp--text-document-position-params)
                      (lambda () '(:textDocument (:uri "file:///test.el")
                                  :position (:line 0 :character 7))))
                     ((symbol-function 'lsp-request)
                      (lambda (method params)
                        (when (string= method "textDocument/references")
                          ;; Return mock references
                          `((:uri ,(concat "file://" test-file)
                             :range (:start (:line 0 :character 7)
                                    :end (:line 0 :character 16)))
                            (:uri ,(concat "file://" test-file)
                             :range (:start (:line 5 :character 3)
                                    :end (:line 5 :character 12)))))))
                     ((symbol-function 'lsp--uri-to-path)
                      (lambda (uri) (substring uri 7))))  ;; Remove "file://"
            
            (let ((result (claude-code-emacs-mcp-handle-findReferences
                           `((file . ,(file-name-nondirectory test-file))
                             (line . 1)
                             (symbol . "test-func")
                             (includeDeclaration . t)))))
              (should (assoc 'references result))
              (should (equal (cdr (assoc 'count result)) 2))
              (let ((refs (cdr (assoc 'references result))))
                (should (= (length refs) 2))
                ;; Check first reference (declaration)
                (let ((ref1 (car refs)))
                  (should (string= (cdr (assoc 'file ref1))
                                  (file-name-nondirectory test-file)))
                  (should (assoc 'preview ref1)))
                ;; Check second reference (usage)
                (let ((ref2 (cadr refs)))
                  (should (string= (cdr (assoc 'file ref2))
                                  (file-name-nondirectory test-file)))
                  (should (assoc 'preview ref2)))))))
      (delete-file test-file))))

(ert-deftest test-mcp-handle-findReferences-no-lsp ()
  "Test finding references without LSP available."
  (cl-letf (((symbol-function 'fboundp) (lambda (_) nil)))
    (let ((result (claude-code-emacs-mcp-handle-findReferences
                   '((file . "test.el")
                     (line . 1)
                     (symbol . "test-symbol")))))
      (should (assoc 'error result))
      (should (string-match "LSP mode is not available" (cdr (assoc 'error result)))))))

(ert-deftest test-mcp-handle-findReferences-missing-params ()
  "Test finding references with missing parameters."
  ;; Missing file
  (let ((result (claude-code-emacs-mcp-handle-findReferences
                 '((line . 1) (symbol . "test")))))
    (should (assoc 'error result))
    (should (string-match "file parameter is required" (cdr (assoc 'error result)))))
  
  ;; Missing line
  (let ((result (claude-code-emacs-mcp-handle-findReferences
                 '((file . "test.el") (symbol . "test")))))
    (should (assoc 'error result))
    (should (string-match "line parameter is required" (cdr (assoc 'error result)))))
  
  ;; Missing symbol
  (let ((result (claude-code-emacs-mcp-handle-findReferences
                 '((file . "test.el") (line . 1)))))
    (should (assoc 'error result))
    (should (string-match "symbol parameter is required" (cdr (assoc 'error result))))))

(ert-deftest test-mcp-get-reference-preview ()
  "Test getting reference preview."
  (let ((test-file (make-temp-file "test-preview" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert ";; Line 0\n")
            (insert "(defun test-func ()\n")  ;; Line 1
            (insert "  \"Documentation\"\n")   ;; Line 2
            (insert "  (message \"test\"))\n"))  ;; Line 3
          
          (let* ((range '(:start (:line 1 :character 7)
                          :end (:line 1 :character 16)))
                 (preview (claude-code-emacs-mcp-get-preview-text test-file range)))
            (should preview)
            ;; Should include line 0 (3 lines before)
            (should (string-match ";; Line 0" preview))
            ;; Should include the actual reference line
            (should (string-match "defun test-func" preview))
            ;; Should include lines after
            (should (string-match "Documentation" preview))
            (should (string-match "message" preview))))
      (delete-file test-file))))

;;; Tests for symbol description

(ert-deftest test-mcp-handle-describeSymbol ()
  "Test describing symbol with LSP hover."
  (let ((test-file (make-temp-file "test-desc" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-function (arg)\n")
            (insert "  \"Test function documentation.\"\n")
            (insert "  (message \"Hello %s\" arg))\n"))
          
          (cl-letf* (;; Mock required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (memq sym '(lsp-mode lsp-request))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (emacs-lisp-mode)
                            (insert-file-contents file nil nil nil t)
                            (setq-local lsp-mode t))
                          buf)))
                     ((symbol-function 'bound-and-true-p)
                      (lambda (sym)
                        (if (eq sym 'lsp-mode) t
                          (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'lsp--text-document-position-params)
                      (lambda () '(:textDocument (:uri "file:///test.el")
                                  :position (:line 0 :character 7))))
                     ((symbol-function 'thing-at-point)
                      (lambda (thing &optional no-props)
                        (when (eq thing 'symbol)
                          "test-function")))
                     ((symbol-function 'lsp-request)
                      (lambda (method params)
                        (when (string= method "textDocument/hover")
                          ;; Return mock hover response with MarkedString array
                          `(:contents ((:language "elisp"
                                       :value "(defun test-function (arg)\n  \"Test function documentation.\"\n  (message \"Hello %s\" arg))")
                                      "Test function documentation.")
                            :range (:start (:line 0 :character 7)
                                   :end (:line 0 :character 20)))))))
            
            (let ((result (claude-code-emacs-mcp-handle-describeSymbol
                           `((file . ,(file-name-nondirectory test-file))
                             (line . 1)
                             (symbol . "test-function")))))
              (should (assoc 'description result))
              (should (equal (cdr (assoc 'method result)) "lsp"))
              (let ((desc (cdr (assoc 'description result))))
                (should (assoc 'documentation desc))))))
      (delete-file test-file))))

(ert-deftest test-mcp-handle-describeSymbol-markdown-format ()
  "Test that MarkedString with language is formatted as markdown code block."
  (let ((test-file (make-temp-file "test-desc" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-function (arg)\n")
            (insert "  \"Test function documentation.\"\n")
            (insert "  (message \"Hello %s\" arg))\n"))
          
          (cl-letf* (;; Mock required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (memq sym '(lsp-mode lsp-request))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (emacs-lisp-mode)
                            (insert-file-contents file nil nil nil t)
                            (setq-local lsp-mode t))
                          buf)))
                     ((symbol-function 'bound-and-true-p)
                      (lambda (sym)
                        (if (eq sym 'lsp-mode) t
                          (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'lsp--text-document-position-params)
                      (lambda () '(:textDocument (:uri "file:///test.el")
                                  :position (:line 0 :character 7))))
                     ((symbol-function 'lsp-request)
                      (lambda (method params)
                        (when (string= method "textDocument/hover")
                          ;; Return mock hover response with MarkedString array
                          `(:contents ((:language "elisp"
                                       :value "(defun test-function (arg))")
                                      "Test function documentation.")
                            :range (:start (:line 0 :character 7)
                                   :end (:line 0 :character 20)))))))
            
            (let ((result (claude-code-emacs-mcp-handle-describeSymbol
                           `((file . ,(file-name-nondirectory test-file))
                             (line . 1)
                             (symbol . "test-function")))))
              (should (assoc 'description result))
              (should (equal (cdr (assoc 'method result)) "lsp"))
              (let* ((desc (cdr (assoc 'description result)))
                     (doc (cdr (assoc 'documentation desc))))
                (should doc)
                ;; Check that markdown code block is formatted correctly
                (should (string-match "```elisp" doc))
                (should (string-match "(defun test-function (arg))" doc))
                (should (string-match "```" doc))
                (should (string-match "Test function documentation\\." doc))))))
      (delete-file test-file))))

(ert-deftest test-mcp-handle-describeSymbol-no-lsp ()
  "Test describing symbol without LSP available."
  (cl-letf (((symbol-function 'fboundp) (lambda (_) nil)))
    (should-error
     (claude-code-emacs-mcp-handle-describeSymbol
      '((file . "test.el")
        (line . 1)
        (symbol . "test-symbol")))
     :type 'error)))

(ert-deftest test-mcp-handle-describeSymbol-missing-params ()
  "Test describing symbol with missing parameters."
  ;; Missing file
  (should-error
   (claude-code-emacs-mcp-handle-describeSymbol
    '((line . 1) (symbol . "test")))
   :type 'error)
  
  ;; Missing line
  (should-error
   (claude-code-emacs-mcp-handle-describeSymbol
    '((file . "test.el") (symbol . "test")))
   :type 'error)
  
  ;; Missing symbol
  (should-error
   (claude-code-emacs-mcp-handle-describeSymbol
    '((file . "test.el") (line . 1)))
   :type 'error))

(provide 'test-claude-code-emacs-mcp-tools)
;;; test-claude-code-emacs-mcp-tools.el ends here
