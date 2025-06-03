;;; test-claude-code-emacs-mcp.el --- Tests for MCP integration -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP server integration

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp)
(require 'cl-lib)

;;; Test utilities

(defmacro claude-code-emacs-mcp-test-with-server (&rest body)
  "Execute BODY with MCP server mocked."
  `(cl-letf* ((claude-code-emacs-mcp-server-buffer "*test-mcp-server*")
              (claude-code-emacs-mcp-process nil)
              (claude-code-emacs-mcp-connection nil)
              ((symbol-function 'make-process)
               (lambda (&rest args)
                 (let ((proc (make-process-mock)))
                   (setq claude-code-emacs-mcp-process proc)
                   proc)))
              ((symbol-function 'open-network-stream)
               (lambda (&rest args)
                 (let ((conn (make-network-mock)))
                   (setq claude-code-emacs-mcp-connection conn)
                   conn)))
              ((symbol-function 'process-live-p)
               (lambda (proc)
                 (and proc (consp proc) (eq (car proc) 'mock-process))))
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
              ((symbol-function 'kill-process)
               (lambda (proc)
                 (when (consp proc)
                   (setcar proc 'dead-process))))
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

;;; Server management tests

(ert-deftest test-mcp-server-start ()
  "Test starting the MCP server."
  (claude-code-emacs-mcp-test-with-server
   (cl-letf (((symbol-function 'sleep-for) (lambda (n))))
     (claude-code-emacs-mcp-start-server)
     (should (claude-code-emacs-mcp-server-running-p))
     (should claude-code-emacs-mcp-process)
     (should claude-code-emacs-mcp-connection))))

(ert-deftest test-mcp-server-stop ()
  "Test stopping the MCP server."
  (claude-code-emacs-mcp-test-with-server
   (cl-letf (((symbol-function 'sleep-for) (lambda (n))))
     (claude-code-emacs-mcp-start-server)
     (claude-code-emacs-mcp-stop-server)
     (should-not (claude-code-emacs-mcp-server-running-p))
     (should-not claude-code-emacs-mcp-process)
     (should-not claude-code-emacs-mcp-connection))))

;;; JSON-RPC communication tests

(ert-deftest test-mcp-send-request ()
  "Test sending JSON-RPC request."
  (claude-code-emacs-mcp-test-with-server
   (cl-letf* ((sent-data nil)
              ((symbol-function 'process-send-string)
               (lambda (conn data)
                 (setq sent-data data)))
              ((symbol-function 'sleep-for) (lambda (n))))
     (claude-code-emacs-mcp-start-server)
     
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
    (with-temp-buffer
      (setq buffer-file-name "/test/project/file1.el")
      (let ((result (claude-code-emacs-mcp-handle-getOpenBuffers '((includeHidden . nil)))))
        (should (assoc 'buffers result))
        (let ((buffers (cdr (assoc 'buffers result))))
          (should (> (length buffers) 0))
          (let ((buffer-info (car buffers)))
            (should (assoc 'path buffer-info))
            (should (assoc 'name buffer-info))
            (should (assoc 'active buffer-info))
            (should (assoc 'modified buffer-info))))))))

(ert-deftest test-mcp-handle-getCurrentSelection ()
  "Test getCurrentSelection handler."
  (with-temp-buffer
    (insert "Line 1\nLine 2\nLine 3")
    (goto-char (point-min))
    (push-mark (point-max) t t)
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

(ert-deftest test-mcp-ensure-server ()
  "Test server auto-start functionality."
  (claude-code-emacs-mcp-test-with-server
   (cl-letf (((symbol-function 'sleep-for) (lambda (n))))
     (should-not (claude-code-emacs-mcp-server-running-p))
     (claude-code-emacs-mcp-ensure-server)
     (should (claude-code-emacs-mcp-server-running-p)))))

(provide 'test-claude-code-emacs-mcp)
;;; test-claude-code-emacs-mcp.el ends here