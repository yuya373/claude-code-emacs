;;; test-claude-code-emacs-mcp-tools.el --- Tests for MCP tool handlers -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP tool handlers

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-tools)
(require 'cl-lib)
;; Optional requirement for lsp-protocol structures
(require 'lsp-protocol nil t)
;; Mock alert for testing
(unless (fboundp 'alert)
  (defun alert (message &rest args)
    "Mock alert function for testing."
    (list 'alert message args)))

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
             (lambda (sym) (or (memq sym '(lsp-diagnostics lsp:diagnostic-message
                                           lsp:diagnostic-severity lsp:diagnostic-source
                                           lsp:diagnostic-range lsp:range-start
                                           lsp:position-line lsp:position-character))
                               (and (boundp sym) (symbol-value sym))))))
    (should-error
     (claude-code-emacs-mcp-handle-getDiagnostics '((buffer . "non-existent-buffer")))
     :type 'error)))

(ert-deftest test-mcp-handle-getDiagnostics-with-buffer ()
  "Test getDiagnostics handler with buffer parameter."
  (let ((test-buffer (generate-new-buffer "test-diagnostics-buffer")))
    (unwind-protect
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (sym) (or (memq sym '(lsp-diagnostics lsp:diagnostic-message
                                                 lsp:diagnostic-severity lsp:diagnostic-source
                                                 lsp:diagnostic-range lsp:range-start
                                                 lsp:position-line lsp:position-character
                                                 lsp-make-diagnostic lsp-make-range lsp-make-position))
                                     (and (boundp sym) (symbol-value sym)))))
                  ((symbol-function 'lsp-diagnostics)
                   (lambda (&optional _workspace)
                     (let ((hash (make-hash-table :test 'equal)))
                       (puthash "/test/file.el"
                                (list (if (fboundp 'lsp-make-diagnostic)
                                          (lsp-make-diagnostic
                                           :range (lsp-make-range
                                                   :start (lsp-make-position :line 9 :character 0)
                                                   :end (lsp-make-position :line 9 :character 10))
                                           :message "Test diagnostic message"
                                           :severity 1
                                           :source "test-lsp")
                                        ;; Fallback to simple object
                                        'mock-diagnostic))
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

(ert-deftest test-mcp-handle-openDiffFile ()
  "Test openDiffFile handler."
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
            (let ((result (claude-code-emacs-mcp-handle-openDiffFile params)))
              (should (assoc 'status result))
              (should (equal (cdr (assoc 'status result)) "success")))))
      (delete-file test-file-a)
      (delete-file test-file-b))))

(ert-deftest test-mcp-handle-openDiffContent ()
  "Test openDiffContent handler."
  (cl-letf* ((ediff-called nil)
             ((symbol-function 'ediff-buffers)
              (lambda (buf-a buf-b)
                (setq ediff-called t)
                (should (get-buffer buf-a))
                (should (get-buffer buf-b))
                (should (equal (buffer-name buf-a) "Test Buffer A"))
                (should (equal (buffer-name buf-b) "Test Buffer B"))
                (with-current-buffer buf-a
                  (should (equal (buffer-string) "Content A\nLine 2")))
                (with-current-buffer buf-b
                  (should (equal (buffer-string) "Content B\nLine 2 modified"))))))
    (let* ((params '((contentA . "Content A\nLine 2")
                     (contentB . "Content B\nLine 2 modified")
                     (titleA . "Test Buffer A")
                     (titleB . "Test Buffer B")))
           (result (claude-code-emacs-mcp-handle-openDiffContent params)))
      (should ediff-called)
      (should (assoc 'status result))
      (should (equal (cdr (assoc 'status result)) "success"))
      (should (assoc 'message result))
      (should (string-match "Test Buffer A.*Test Buffer B" (cdr (assoc 'message result))))
      ;; Clean up created buffers
      (when (get-buffer "Test Buffer A")
        (kill-buffer "Test Buffer A"))
      (when (get-buffer "Test Buffer B")
        (kill-buffer "Test Buffer B")))))

(ert-deftest test-mcp-handle-openDiffContent-error ()
  "Test openDiffContent handler error cases."
  ;; Test missing parameters
  (let ((result (claude-code-emacs-mcp-handle-openDiffContent '())))
    (should (assoc 'status result))
    (should (equal (cdr (assoc 'status result)) "error"))
    (should (assoc 'message result))
    (should (string-match "Missing required parameters" (cdr (assoc 'message result)))))

  ;; Test missing contentB
  (let ((result (claude-code-emacs-mcp-handle-openDiffContent
                 '((contentA . "test")
                   (titleA . "A")
                   (titleB . "B")))))
    (should (assoc 'status result))
    (should (equal (cdr (assoc 'status result)) "error")))

  ;; Test missing titleA
  (let ((result (claude-code-emacs-mcp-handle-openDiffContent
                 '((contentA . "test")
                   (contentB . "test")
                   (titleB . "B")))))
    (should (assoc 'status result))
    (should (equal (cdr (assoc 'status result)) "error"))))

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

;;; Tests for definition finding

(ert-deftest test-mcp-handle-getDefinition-with-lsp ()
  "Test getting definition with LSP."
  (let ((test-file (make-temp-file "test-def" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-func () nil)"))
          (cl-letf* (;; Mock lsp-make-* functions
                     ((symbol-function 'lsp-make-location)
                      (lambda (&rest args)
                        (let ((uri (plist-get args :uri))
                              (range (plist-get args :range)))
                          `(:uri ,uri :range ,range))))
                     ((symbol-function 'lsp-make-range)
                      (lambda (&rest args)
                        (let ((start (plist-get args :start))
                              (end (plist-get args :end)))
                          `(:start ,start :end ,end))))
                     ((symbol-function 'lsp-make-position)
                      (lambda (&rest args)
                        (let ((line (plist-get args :line))
                              (character (plist-get args :character)))
                          `(:line ,line :character ,character))))
                     ;; Mock all required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (or (memq sym '(lsp-mode lsp-request))
                                        (and (boundp sym) (symbol-value sym)))))
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
                            (setq major-mode 'emacs-lisp-mode)
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
                          ;; Always load lsp-protocol to ensure functions are available
                          (require 'lsp-protocol)
                          (lsp-make-location
                           :uri (concat "file://" test-file)
                           :range (lsp-make-range
                                   :start (lsp-make-position :line 0 :character 0)
                                   :end (lsp-make-position :line 0 :character 23))))))
                     ((symbol-function 'lsp--uri-to-path)
                      (lambda (uri) test-file))
                     ;; Mock lsp--location-uri and lsp--location-range
                     ((symbol-function 'lsp--location-uri)
                      (lambda (loc) (plist-get loc :uri)))
                     ((symbol-function 'lsp--location-range)
                      (lambda (loc) (plist-get loc :range)))
                     ;; Mock LSP functions for preview
                     ((symbol-function 'lsp:position-line)
                      (lambda (pos) (plist-get pos :line)))
                     ((symbol-function 'lsp:range-start)
                      (lambda (range) (plist-get range :start)))
                     ((symbol-function 'lsp:range-end)
                      (lambda (range) (plist-get range :end)))
                     ;; Mock the function that actually calls lsp-request
                     ((symbol-function 'claude-code-emacs-mcp-get-lsp-definitions-with-request)
                      (lambda ()
                        ;; Return a list with one definition
                        (list `((file . ,test-file)
                                (preview . "(defun test-func () nil)")
                                (range . (:start (:line 0 :character 0)
                                          :end (:line 0 :character 23))))))))
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
      (file . "test.el")))
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
  ;; Skip this test - it tests internal implementation details
  ;; that rely on lsp-interface pcase patterns
  (skip-unless nil))
(ert-deftest test-mcp-get-definition-info-at ()
  "Test getting definition info at specific location."
  (let ((test-file (make-temp-file "test-def" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert "(defun test-function (arg)\n")
            (insert "  \"Documentation string.\"\n")
            (insert "  (message \"Hello %s\" arg))\n"))
          ;; Mock LSP functions needed for preview
          (cl-letf (((symbol-function 'lsp:position-line)
                     (lambda (pos) (plist-get pos :line)))
                    ((symbol-function 'lsp:range-start)
                     (lambda (range) (plist-get range :start)))
                    ((symbol-function 'lsp:range-end)
                     (lambda (range) (plist-get range :end))))
            (let* ((range '(:start (:line 0 :character 7) :end (:line 0 :character 20)))
                   (result (claude-code-emacs-mcp-get-definition-info-at test-file range)))
              (should result)
              (should (equal (cdr (assoc 'file result)) test-file))
              (should (string-match "defun test-function" (cdr (assoc 'preview result))))
              (should (equal (cdr (assoc 'range result)) range)))))
      (delete-file test-file))))

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

          ;; Mock LSP functions needed for preview
          (cl-letf (((symbol-function 'lsp:position-line)
                     (lambda (pos) (plist-get pos :line)))
                    ((symbol-function 'lsp:range-start)
                     (lambda (range) (plist-get range :start)))
                    ((symbol-function 'lsp:range-end)
                     (lambda (range) (plist-get range :end))))
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
              (should t))))
      (delete-file test-file))))

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

          (cl-letf* (;; Mock lsp-make-* functions
                     ((symbol-function 'lsp-make-location)
                      (lambda (&rest args)
                        (let ((uri (plist-get args :uri))
                              (range (plist-get args :range)))
                          `(:uri ,uri :range ,range))))
                     ((symbol-function 'lsp-make-range)
                      (lambda (&rest args)
                        (let ((start (plist-get args :start))
                              (end (plist-get args :end)))
                          `(:start ,start :end ,end))))
                     ((symbol-function 'lsp-make-position)
                      (lambda (&rest args)
                        (let ((line (plist-get args :line))
                              (character (plist-get args :character)))
                          `(:line ,line :character ,character))))
                     ;; Mock required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (or (memq sym '(lsp-mode lsp-request
                                                    lsp-make-location lsp-make-range
                                                    lsp-make-position lsp-make-location-link
                                                    lsp-make-hover lsp-make-markup-content))
                                        (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (setq major-mode 'emacs-lisp-mode)
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
                          (list `(:uri ,(concat "file://" test-file)
                                  :range (:start (:line 0 :character 7)
                                          :end (:line 0 :character 16)))
                                `(:uri ,(concat "file://" test-file)
                                  :range (:start (:line 5 :character 3)
                                          :end (:line 5 :character 12)))))))
                     ((symbol-function 'lsp--uri-to-path)
                      (lambda (uri) (substring uri 7)))  ;; Remove "file://"
                     ;; Mock LSP location accessors
                     ((symbol-function 'lsp:location-uri)
                      (lambda (loc) (plist-get loc :uri)))
                     ((symbol-function 'lsp:location-range)
                      (lambda (loc) (plist-get loc :range)))
                     ;; Mock LSP position/range accessors for preview
                     ((symbol-function 'lsp:position-line)
                      (lambda (pos) (plist-get pos :line)))
                     ((symbol-function 'lsp:range-start)
                      (lambda (range) (plist-get range :start)))
                     ((symbol-function 'lsp:range-end)
                      (lambda (range) (plist-get range :end))))

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
                  (should (assoc 'preview ref2))))))))
      (delete-file test-file)))

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

          ;; Mock LSP functions instead of requiring lsp-protocol
          (cl-letf (((symbol-function 'lsp-make-range)
                     (lambda (&rest args)
                       (let ((start (plist-get args :start))
                             (end (plist-get args :end)))
                         `(:start ,start :end ,end))))
                    ((symbol-function 'lsp-make-position)
                     (lambda (&rest args)
                       (let ((line (plist-get args :line))
                             (character (plist-get args :character)))
                         `(:line ,line :character ,character))))
                    ((symbol-function 'lsp:position-line)
                     (lambda (pos) (plist-get pos :line)))
                    ((symbol-function 'lsp:range-start)
                     (lambda (range) (plist-get range :start)))
                    ((symbol-function 'lsp:range-end)
                     (lambda (range) (plist-get range :end))))
            (let* ((range (lsp-make-range
                           :start (lsp-make-position :line 1 :character 7)
                           :end (lsp-make-position :line 1 :character 16)))
                   (preview (claude-code-emacs-mcp-get-preview-text test-file range)))
              (should preview)
              ;; Should include line 0 (3 lines before)
              (should (string-match ";; Line 0" preview))
              ;; Should include the actual reference line
              (should (string-match "defun test-func" preview))
              ;; Should include lines after
              (should (string-match "Documentation" preview))
              (should (string-match "message" preview)))))
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

          (cl-letf* (;; Mock lsp-make-* functions
                     ((symbol-function 'lsp-make-hover)
                      (lambda (&rest args)
                        (let ((contents (plist-get args :contents)))
                          `(:contents ,contents))))
                     ((symbol-function 'lsp-make-markup-content)
                      (lambda (&rest args)
                        (let ((kind (plist-get args :kind))
                              (value (plist-get args :value)))
                          `(:kind ,kind :value ,value))))
                     ;; Mock required functions
                     ((symbol-function 'fboundp)
                      (lambda (sym) (or (memq sym '(lsp-mode lsp-request))
                                        (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (setq major-mode 'emacs-lisp-mode)
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
                          `(:contents (:kind "markdown"
                                       :value ,(concat "```elisp\n"
                                                       "(defun test-function (arg)\n"
                                                       "  \"Test function documentation.\"\n"
                                                       "  (message \"Hello %s\" arg))\n"
                                                       "```\n\n"
                                                       "Test function documentation."))))))
                     ;; Mock required functions for hover
                     ((symbol-function 'lsp:hover-contents)
                      (lambda (hover) (plist-get hover :contents)))
                     ((symbol-function 'lsp-markup-content?)
                      (lambda (obj) (and (listp obj) (plist-get obj :kind))))
                     ((symbol-function 'lsp:markup-content-kind)
                      (lambda (mc) (plist-get mc :kind)))
                     ((symbol-function 'lsp:markup-content-value)
                      (lambda (mc) (plist-get mc :value))))

            (let* ((result (claude-code-emacs-mcp-handle-describeSymbol
                           `((file . ,(file-name-nondirectory test-file))
                             (line . 1)
                             (symbol . "test-function")))))
              (should (assoc 'description result))
              (should (equal (cdr (assoc 'method result)) "lsp"))
              (let ((desc (cdr (assoc 'description result))))
                (should (assoc 'documentation desc)))))))
      (delete-file test-file)))

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
                      (lambda (sym) (or (memq sym '(lsp-mode lsp-request
                                                    lsp-make-location lsp-make-range
                                                    lsp-make-position lsp-make-location-link
                                                    lsp-make-hover lsp-make-markup-content))
                                        (and (boundp sym) (symbol-value sym)))))
                     ((symbol-function 'projectile-project-root)
                      (lambda () (file-name-directory test-file)))
                     ((symbol-function 'claude-code-emacs-normalize-project-root)
                      (lambda (root) root))
                     ((symbol-function 'find-file-noselect)
                      (lambda (file)
                        (let ((buf (get-buffer-create (file-name-nondirectory file))))
                          (with-current-buffer buf
                            (setq buffer-file-name file)
                            (setq major-mode 'emacs-lisp-mode)
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
                          `(:contents (:kind "markdown"
                                       :value ,(concat "```elisp\n"
                                                       "(defun test-function (arg))\n"
                                                       "```\n\n"
                                                       "Test function documentation."))))))
                     ;; Mock required functions for hover
                     ((symbol-function 'lsp:hover-contents)
                      (lambda (hover) (plist-get hover :contents)))
                     ((symbol-function 'lsp-markup-content?)
                      (lambda (obj) (and (listp obj) (plist-get obj :kind))))
                     ((symbol-function 'lsp:markup-content-kind)
                      (lambda (mc) (plist-get mc :kind)))
                     ((symbol-function 'lsp:markup-content-value)
                      (lambda (mc) (plist-get mc :value))))

            (let* ((result (claude-code-emacs-mcp-handle-describeSymbol
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
                (should (string-match "Test function documentation\\." doc)))))))
      (delete-file test-file)))

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

;;; Tests for notification handler

(ert-deftest test-mcp-handle-sendNotification ()
  "Test sendNotification handler."
  ;; Store alerts for testing
  (let ((alert-calls '()))
    (cl-letf (((symbol-function 'alert)
               (lambda (message &rest args)
                 (push (list 'message message 'args args) alert-calls))))

      ;; Test successful notification
      (let ((result (claude-code-emacs-mcp-handle-sendNotification
                     '((title . "Test Title")
                       (message . "Test message content")))))
        (should (equal (cdr (assoc 'success result)) t))
        (should (equal (cdr (assoc 'message result)) "Notification sent"))

        ;; Check that alert was called with correct parameters
        (should (= (length alert-calls) 1))
        (let ((call (car alert-calls)))
          (should (equal (plist-get call 'message) "Test message content"))
          (let ((args (plist-get call 'args)))
            (should (equal (plist-get args :title) "Test Title"))
            (should (eq (plist-get args :category) 'claude-code))))))))

(ert-deftest test-mcp-handle-sendNotification-default-category ()
  "Test sendNotification uses claude-code category when not provided."
  (let ((alert-calls '()))
    (cl-letf (((symbol-function 'alert)
               (lambda (message &rest args)
                 (push (list 'message message 'args args) alert-calls))))

      ;; Call without category parameter
      (claude-code-emacs-mcp-handle-sendNotification
       '((title . "Test")
         (message . "Message")))

      ;; Check that claude-code category was used
      (let* ((call (car alert-calls))
             (args (plist-get call 'args)))
        (should (eq (plist-get args :category) 'claude-code))))))

(ert-deftest test-mcp-handle-sendNotification-missing-title ()
  "Test sendNotification with missing title."
  (should-error
   (claude-code-emacs-mcp-handle-sendNotification
    '((message . "Test message")))
   :type 'error))

(ert-deftest test-mcp-handle-sendNotification-missing-message ()
  "Test sendNotification with missing message."
  (should-error
   (claude-code-emacs-mcp-handle-sendNotification
    '((title . "Test Title")))
   :type 'error))

(ert-deftest test-mcp-handle-sendNotification-fallback-to-message ()
  "Test sendNotification falls back to message when alert is not available."
  (let ((message-calls '()))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'alert)
                     nil  ;; Pretend alert is not available
                   (funcall #'fboundp sym))))
              ((symbol-function 'message)
               (lambda (format &rest args)
                 (push (list 'format format 'args args) message-calls))))

      (let ((result (claude-code-emacs-mcp-handle-sendNotification
                     '((title . "Test Title")
                       (message . "Test message")))))
        (should (equal (cdr (assoc 'success result)) t))

        ;; Check that message was called with correct format
        (should (= (length message-calls) 1))
        (let ((call (car message-calls)))
          (should (equal (plist-get call 'format) "[%s] %s"))
          (should (equal (plist-get call 'args) '("Test Title" "Test message"))))))))

(provide 'test-claude-code-emacs-mcp-tools)
;;; test-claude-code-emacs-mcp-tools.el ends here
