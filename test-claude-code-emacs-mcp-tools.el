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

(provide 'test-claude-code-emacs-mcp-tools)
;;; test-claude-code-emacs-mcp-tools.el ends here
