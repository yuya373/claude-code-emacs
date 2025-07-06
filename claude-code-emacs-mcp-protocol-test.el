;;; test-claude-code-emacs-mcp-protocol.el --- Tests for MCP JSON-RPC protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP JSON-RPC protocol handling

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-protocol)
(require 'cl-lib)

;;; Test utilities

(defmacro claude-code-emacs-mcp-test-with-connection (&rest body)
  "Execute BODY with MCP connection mocked."
  `(let ((claude-code-emacs-mcp-project-connections (make-hash-table :test 'equal)))
     (cl-letf* (((symbol-function 'websocket-open)
                 (lambda (url &rest args)
                   (let ((ws (cons 'mock-websocket nil))
                         (on-open (plist-get args :on-open)))
                     ;; Call on-open callback immediately
                     (when on-open
                       (funcall on-open ws))
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

(ert-deftest test-mcp-handle-message-ping-pong ()
  "Test handling ping/pong messages."
  (let ((project-root "/test/project/")
        (pong-handled nil))
    (cl-letf (((symbol-function 'claude-code-emacs-mcp-handle-pong)
               (lambda (root)
                 (when (string= root project-root)
                   (setq pong-handled t)))))
      ;; Test pong message
      (claude-code-emacs-mcp-handle-message "{\"type\":\"pong\"}" project-root)
      (should pong-handled))))

(ert-deftest test-mcp-handle-message-request ()
  "Test handling incoming requests."
  (let ((project-root "/test/project/")
        (request-handled nil))
    (cl-letf (((symbol-function 'claude-code-emacs-mcp-handle-request)
               (lambda (request root)
                 (when (and (equal (cdr (assoc 'method request)) "testMethod")
                           (string= root project-root))
                   (setq request-handled t)))))
      ;; Test request message
      (claude-code-emacs-mcp-handle-message
       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"testMethod\",\"params\":{}}"
       project-root)
      (should request-handled))))

(ert-deftest test-mcp-handle-message-response ()
  "Test handling responses to our requests."
  (let* ((project-root "/test/project/")
         (callback-result nil)
         (claude-code-emacs-mcp-project-connections (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'claude-code-emacs-normalize-project-root)
               (lambda (root) root)))
      ;; Set up connection info with pending request
      (let* ((pending-requests (make-hash-table :test 'equal))
             (info `((websocket . nil)
                     (request-id . 1)
                     (pending-requests . ,pending-requests)
                     (connection-attempts . 0)
                     (ping-timer . nil)
                     (ping-timeout-timer . nil)
                     (last-pong-time . nil))))
        (puthash project-root info claude-code-emacs-mcp-project-connections)
        ;; Add pending request
        (puthash 1 (lambda (result error)
                     (setq callback-result (or result error)))
                 pending-requests)
        ;; Handle response
        (claude-code-emacs-mcp-handle-message
         "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"data\":\"test\"}}"
         project-root)
        ;; Check callback was called
        (should (equal callback-result '((data . "test"))))))))

(ert-deftest test-mcp-handle-request ()
  "Test request handling and response."
  (let ((project-root "/test/project/")
        (sent-response nil))
    (cl-letf (((symbol-function 'claude-code-emacs-mcp-handle-getOpenBuffers)
               (lambda (params) '((buffers . ()))))
              ((symbol-function 'claude-code-emacs-mcp-send-response)
               (lambda (id result error root)
                 (setq sent-response (list id result error root)))))
      ;; Handle request
      (claude-code-emacs-mcp-handle-request
       '((id . 123)
         (method . "getOpenBuffers")
         (params . ()))
       project-root)
      ;; Check response was sent
      (should (equal (nth 0 sent-response) 123))
      (should (equal (nth 1 sent-response) '((buffers . ()))))
      (should-not (nth 2 sent-response))
      (should (equal (nth 3 sent-response) project-root)))))

(ert-deftest test-mcp-handle-request-error ()
  "Test request error handling."
  (let ((project-root "/test/project/")
        (sent-response nil))
    (cl-letf (((symbol-function 'claude-code-emacs-mcp-handle-getOpenBuffers)
               (lambda (params) (error "Test error")))
              ((symbol-function 'claude-code-emacs-mcp-send-response)
               (lambda (id result error root)
                 (setq sent-response (list id result error root)))))
      ;; Handle request that will error
      (claude-code-emacs-mcp-handle-request
       '((id . 123)
         (method . "getOpenBuffers")
         (params . ()))
       project-root)
      ;; Check error response was sent
      (should (equal (nth 0 sent-response) 123))
      (should-not (nth 1 sent-response))
      (should (nth 2 sent-response))
      (should (equal (cdr (assoc 'code (nth 2 sent-response))) -32603)))))

(ert-deftest test-mcp-send-response ()
  "Test sending JSON-RPC responses."
  (let ((project-root "/test/project/")
        (sent-text nil)
        (claude-code-emacs-mcp-project-connections (make-hash-table :test 'equal)))
    (cl-letf (((symbol-function 'claude-code-emacs-normalize-project-root)
               (lambda (root) root)))
      ;; Set up mock websocket
      (let* ((mock-ws 'mock-websocket)
             (info `((websocket . ,mock-ws)
                     (request-id . 0)
                     (pending-requests . ,(make-hash-table :test 'equal))
                     (connection-attempts . 0)
                     (ping-timer . nil)
                     (ping-timeout-timer . nil)
                     (last-pong-time . nil))))
        (puthash project-root info claude-code-emacs-mcp-project-connections)
        (cl-letf (((symbol-function 'websocket-send-text)
                   (lambda (ws text)
                     (when (eq ws mock-ws)
                       (setq sent-text text)))))
          ;; Send success response
          (claude-code-emacs-mcp-send-response 123 '((result . "ok")) nil project-root)
          ;; Check JSON structure
          (let* ((json-object-type 'alist)
                 (parsed (json-read-from-string sent-text)))
            (should (equal (cdr (assoc 'jsonrpc parsed)) "2.0"))
            (should (equal (cdr (assoc 'id parsed)) 123))
            (should (equal (cdr (assoc 'result parsed)) '((result . "ok"))))))))))

(provide 'test-claude-code-emacs-mcp-protocol)
;;; test-claude-code-emacs-mcp-protocol.el ends here
