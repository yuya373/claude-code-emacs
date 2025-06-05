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

(provide 'test-claude-code-emacs-mcp-tools)
;;; test-claude-code-emacs-mcp-tools.el ends here
