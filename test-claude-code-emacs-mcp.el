;;; test-claude-code-emacs-mcp.el --- Tests for MCP integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP server integration

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp)
(require 'cl-lib)

;;; Test utilities

(defmacro claude-code-emacs-mcp-test-with-connection (&rest body)
  "Execute BODY with MCP connection mocked."
  `(cl-letf* ((claude-code-emacs-mcp-connection nil)
              ((symbol-function 'open-network-stream)
               (lambda (&rest args)
                 (let ((conn (make-network-mock)))
                   (setq claude-code-emacs-mcp-connection conn)
                   conn)))
              ((symbol-function 'process-live-p)
               (lambda (proc)
                 (and proc (consp proc) (member (car proc) '(mock-connection mock-process)))))
              ((symbol-function 'set-process-filter)
               (lambda (proc filter)
                 (when (consp proc)
                   (put proc 'process-filter filter))))
              ((symbol-function 'set-process-sentinel)
               (lambda (proc sentinel)
                 (when (consp proc)
                   (put proc 'process-sentinel sentinel))))
              ((symbol-function 'process-send-string)
               (lambda (proc string)
                 (when (consp proc)
                   (put proc 'sent-data string))))
              ((symbol-function 'delete-process)
               (lambda (proc)
                 (when (consp proc)
                   (setcar proc 'deleted-process)))))
     ,@body))

(defun make-process-mock ()
  "Create a mock process for testing."
  (let ((proc (cons 'mock-process nil)))
    proc))

(defun make-network-mock ()
  "Create a mock network connection for testing."
  (let ((conn (cons 'mock-connection nil)))
    conn))

(defun mock-receive-response (connection response)
  "Simulate receiving RESPONSE on CONNECTION."
  (let ((filter (get connection 'process-filter)))
    (when filter
      (funcall filter connection (concat (json-encode response) "\n")))))

;;; Connection tests

(ert-deftest test-mcp-connect ()
  "Test connecting to MCP server."
  (claude-code-emacs-mcp-test-with-connection
   (claude-code-emacs-mcp-connect)
   (should claude-code-emacs-mcp-connection)
   (should (process-live-p claude-code-emacs-mcp-connection))))

(ert-deftest test-mcp-disconnect ()
  "Test disconnecting from MCP server."
  (claude-code-emacs-mcp-test-with-connection
   (claude-code-emacs-mcp-connect)
   (claude-code-emacs-mcp-disconnect)
   (should-not (process-live-p claude-code-emacs-mcp-connection))))

;;; JSON-RPC communication tests

(ert-deftest test-mcp-send-request ()
  "Test sending JSON-RPC request."
  (claude-code-emacs-mcp-test-with-connection
   (cl-letf* ((sent-data nil)
              ((symbol-function 'process-send-string)
               (lambda (conn data)
                 (setq sent-data data))))
     (claude-code-emacs-mcp-connect)
     
     (let ((callback-called nil)
           (callback-result nil))
       (claude-code-emacs-mcp-send-request
        "test-method"
        '((param1 . "value1"))
        (lambda (result error)
          (setq callback-called t)
          (setq callback-result result)))
       
       ;; Check request was sent
       (should sent-data)
       (let* ((json-object-type 'alist)
              (request (json-read-from-string (string-trim sent-data))))
         (should (equal (cdr (assoc 'jsonrpc request)) "2.0"))
         (should (equal (cdr (assoc 'method request)) "test-method"))
         (should (equal (cdr (assoc 'params request)) '((param1 . "value1"))))
         (should (numberp (cdr (assoc 'id request)))))))))

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

;;; Integration tests

(ert-deftest test-mcp-ensure-connection ()
  "Test connection auto-establish functionality."
  (claude-code-emacs-mcp-test-with-connection
   (should-not claude-code-emacs-mcp-connection)
   (claude-code-emacs-mcp-ensure-connection)
   (should claude-code-emacs-mcp-connection)
   (should (process-live-p claude-code-emacs-mcp-connection))))

(provide 'test-claude-code-emacs-mcp)
;;; test-claude-code-emacs-mcp.el ends here