;;; test-claude-code-emacs-mcp-protocol.el --- Tests for MCP JSON-RPC protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP JSON-RPC protocol handling

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-protocol)
(require 'cl-lib)

;; Disable auto-connect for tests
(setq claude-code-emacs-mcp-auto-connect nil)

;;; Test utilities

(defmacro claude-code-emacs-mcp-test-with-connection (&rest body)
  "Execute BODY with MCP connection mocked."
  `(let ((claude-code-emacs-mcp-project-ports (make-hash-table :test 'equal))
         (claude-code-emacs-mcp-project-connections (make-hash-table :test 'equal))
         (claude-code-emacs-mcp-connection-callbacks (make-hash-table :test 'equal))
         (claude-code-emacs-mcp-port-wait-timeout 2))  ; Short timeout for tests
     ;; Register default port for test project
     (puthash (projectile-project-root) 8766 claude-code-emacs-mcp-project-ports)
     (cl-letf* (((symbol-function 'websocket-open)
                 (lambda (&rest args)
                   (let ((ws (cons 'mock-websocket nil)))
                     ws)))
                ((symbol-function 'websocket-openp)
                 (lambda (ws)
                   (and ws (consp ws) (eq (car ws) 'mock-websocket))))
                ((symbol-function 'websocket-send-text)
                 (lambda (ws text)
                   (when (consp ws)
                     (setf (get ws 'sent-data) text))))
                ((symbol-function 'websocket-close)
                 (lambda (ws)
                   (when (consp ws)
                     (setcar ws 'closed-websocket))))
                ((symbol-function 'sleep-for) (lambda (seconds) nil))
                ((symbol-function 'run-at-time) (lambda (&rest args) nil)))
       ,@body)))

(defun mock-receive-response (connection response)
  "Simulate receiving RESPONSE on CONNECTION."
  (let ((filter (get connection 'process-filter)))
    (when filter
      (funcall filter connection (concat (json-encode response) "\n")))))

;;; JSON-RPC communication tests

(ert-deftest test-mcp-send-request ()
  "Test sending JSON-RPC request."
  (claude-code-emacs-mcp-test-with-connection
   (let ((sent-data nil))
     (cl-letf (((symbol-function 'websocket-send-text)
                (lambda (ws text)
                  (setq sent-data text))))
       (let ((project-root (projectile-project-root)))
         (claude-code-emacs-mcp-connect project-root nil)

         (let ((callback-called nil)
               (callback-result nil))
           (claude-code-emacs-mcp-send-request
            "test-method"
            '((param1 . "value1"))
            (lambda (result error)
              (setq callback-called t)
              (setq callback-result result))
            project-root))

         ;; Check request was sent
         (should sent-data)
         (let* ((json-object-type 'alist)
                (request (json-read-from-string (string-trim sent-data))))
           (should (equal (cdr (assoc 'jsonrpc request)) "2.0"))
           (should (equal (cdr (assoc 'method request)) "test-method"))
           (should (equal (cdr (assoc 'params request)) '((param1 . "value1"))))
           (should (numberp (cdr (assoc 'id request))))))))))

(provide 'test-claude-code-emacs-mcp-protocol)
;;; test-claude-code-emacs-mcp-protocol.el ends here
