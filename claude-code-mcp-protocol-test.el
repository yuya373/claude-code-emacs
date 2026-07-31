;;; test-claude-code-mcp-protocol.el --- Tests for MCP JSON-RPC protocol -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP JSON-RPC protocol handling.
;; Messages are routed per server instance so that multiple Claude Code
;; sessions in the same project do not interfere with each other.

;;; Code:

(require 'ert)
(require 'claude-code-mcp-protocol)
(require 'claude-code-mcp-connection)
(require 'cl-lib)

;;; Test utilities

(defvar claude-code-mcp-test-sent-messages nil
  "List of (WEBSOCKET . TEXT) sent during a test.")

(defvar claude-code-mcp-test-closed-sockets nil
  "List of mock websockets closed during a test.")

(defmacro claude-code-mcp-test-with-connection (&rest body)
  "Execute BODY with MCP connection mocked."
  `(let ((claude-code-mcp-connections (make-hash-table :test 'equal))
         (claude-code-mcp-test-sent-messages nil)
         (claude-code-mcp-test-closed-sockets nil))
     (cl-letf* (((symbol-function 'websocket-open)
                 (lambda (url &rest args)
                   (let ((ws (list 'mock-websocket url))
                         (on-open (plist-get args :on-open)))
                     (when on-open
                       (funcall on-open ws))
                     ws)))
                ((symbol-function 'websocket-openp)
                 (lambda (ws)
                   (and ws (consp ws)
                        (eq (car ws) 'mock-websocket)
                        (not (memq ws claude-code-mcp-test-closed-sockets)))))
                ((symbol-function 'websocket-send-text)
                 (lambda (ws text)
                   (push (cons ws text) claude-code-mcp-test-sent-messages)))
                ((symbol-function 'websocket-close)
                 (lambda (ws)
                   (push ws claude-code-mcp-test-closed-sockets)))
                ((symbol-function 'sleep-for) (lambda (_seconds) nil))
                ((symbol-function 'run-at-time) (lambda (&rest _args) nil)))
       ,@body)))

(defun claude-code-mcp-test-messages-for (ws)
  "Return list of texts sent to WS during the test."
  (mapcar #'cdr
          (cl-remove-if-not (lambda (entry) (eq (car entry) ws))
                            claude-code-mcp-test-sent-messages)))

;;; Message dispatch tests

(ert-deftest test-mcp-handle-message-ping-pong ()
  "Test handling ping/pong messages routes to the instance."
  (let ((pong-handled nil))
    (cl-letf (((symbol-function 'claude-code-mcp-handle-pong)
               (lambda (instance-id)
                 (when (string= instance-id "inst-a")
                   (setq pong-handled t)))))
      (claude-code-mcp-handle-message "{\"type\":\"pong\"}" "inst-a")
      (should pong-handled))))

(ert-deftest test-mcp-handle-message-request ()
  "Test handling incoming requests routes to the instance."
  (let ((request-handled nil))
    (cl-letf (((symbol-function 'claude-code-mcp-handle-request)
               (lambda (request instance-id)
                 (when (and (equal (cdr (assoc 'method request)) "testMethod")
                            (string= instance-id "inst-a"))
                   (setq request-handled t)))))
      (claude-code-mcp-handle-message
       "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"testMethod\",\"params\":{}}"
       "inst-a")
      (should request-handled))))

(ert-deftest test-mcp-handle-message-response ()
  "Test handling responses resolves the instance's pending request."
  (claude-code-mcp-test-with-connection
   (let ((callback-result nil))
     (claude-code-mcp-initialize-connection-info "inst-a" "/test/project" 1111)
     (let* ((info (claude-code-mcp-get-connection-info "inst-a"))
            (pending-requests (cdr (assoc 'pending-requests info))))
       (puthash 1 (lambda (result error)
                    (setq callback-result (or result error)))
                pending-requests)
       (claude-code-mcp-handle-message
        "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{\"data\":\"test\"}}"
        "inst-a")
       (should (equal callback-result '((data . "test"))))))))

;;; Request handling tests

(ert-deftest test-mcp-handle-request ()
  "Test request handling and response."
  (let ((sent-response nil))
    (cl-letf (((symbol-function 'claude-code-mcp-handle-getOpenBuffers)
               (lambda (_params) '((buffers . ()))))
              ((symbol-function 'claude-code-mcp-send-response)
               (lambda (id result error instance-id)
                 (setq sent-response (list id result error instance-id)))))
      (claude-code-mcp-handle-request
       '((id . 123)
         (method . "getOpenBuffers")
         (params . ()))
       "inst-a")
      (should (equal (nth 0 sent-response) 123))
      (should (equal (nth 1 sent-response) '((buffers . ()))))
      (should-not (nth 2 sent-response))
      (should (equal (nth 3 sent-response) "inst-a")))))

(ert-deftest test-mcp-handle-request-error ()
  "Test request error handling."
  (let ((sent-response nil))
    (cl-letf (((symbol-function 'claude-code-mcp-handle-getOpenBuffers)
               (lambda (_params) (error "Test error")))
              ((symbol-function 'claude-code-mcp-send-response)
               (lambda (id result error instance-id)
                 (setq sent-response (list id result error instance-id)))))
      (claude-code-mcp-handle-request
       '((id . 123)
         (method . "getOpenBuffers")
         (params . ()))
       "inst-a")
      (should (equal (nth 0 sent-response) 123))
      (should-not (nth 1 sent-response))
      (should (nth 2 sent-response))
      (should (equal (cdr (assoc 'code (nth 2 sent-response))) -32603)))))

;;; Response routing tests

(ert-deftest test-mcp-send-response ()
  "Test sending JSON-RPC responses."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/test/project" 1111 "inst-a")
   (setq claude-code-mcp-test-sent-messages nil)

   (claude-code-mcp-send-response 123 '((result . "ok")) nil "inst-a")

   (let ((msgs (claude-code-mcp-test-messages-for
                (claude-code-mcp-get-websocket "inst-a"))))
     (should (= 1 (length msgs)))
     (let* ((json-object-type 'alist)
            (parsed (json-read-from-string (car msgs))))
       (should (equal (cdr (assoc 'jsonrpc parsed)) "2.0"))
       (should (equal (cdr (assoc 'id parsed)) 123))
       (should (equal (cdr (assoc 'result parsed)) '((result . "ok"))))))))

(ert-deftest test-mcp-send-response-routes-to-own-instance ()
  "Responses go to the instance the request came from, not another
instance of the same project."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/test/project" 1111 "inst-a")
   (claude-code-mcp-register-port "/test/project" 2222 "inst-b")
   (setq claude-code-mcp-test-sent-messages nil)

   (claude-code-mcp-send-response 1 '((pong . t)) nil "inst-a")

   (let ((msgs-a (claude-code-mcp-test-messages-for
                  (claude-code-mcp-get-websocket "inst-a")))
         (msgs-b (claude-code-mcp-test-messages-for
                  (claude-code-mcp-get-websocket "inst-b"))))
     (should (= 1 (length msgs-a)))
     (should (= 0 (length msgs-b))))))

;;; WebSocket close handling tests

(ert-deftest test-mcp-on-close-removes-own-instance ()
  "Closing the current socket removes only its own instance."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/test/project" 1111 "inst-a")
   (claude-code-mcp-register-port "/test/project" 2222 "inst-b")
   (let ((ws-a (claude-code-mcp-get-websocket "inst-a")))
     (claude-code-mcp-on-close ws-a "inst-a")
     ;; inst-a is cleaned up
     (should-not (claude-code-mcp-get-connection-info "inst-a"))
     ;; inst-b is untouched
     (should (claude-code-mcp-get-connection-info "inst-b"))
     (should (websocket-openp (claude-code-mcp-get-websocket "inst-b"))))))

(ert-deftest test-mcp-on-close-ignores-stale-socket ()
  "A stale socket closing must not tear down the current connection."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/test/project" 1111 "inst-a")
   (let ((old-ws (claude-code-mcp-get-websocket "inst-a")))
     ;; Same instance reconnects on a new port; old-ws becomes stale
     (claude-code-mcp-register-port "/test/project" 3333 "inst-a")
     (let ((new-ws (claude-code-mcp-get-websocket "inst-a")))
       ;; The stale socket's close event arrives late
       (claude-code-mcp-on-close old-ws "inst-a")
       ;; Current connection survives
       (should (claude-code-mcp-get-connection-info "inst-a"))
       (should (eq new-ws (claude-code-mcp-get-websocket "inst-a")))
       (should (websocket-openp new-ws))))))

(provide 'test-claude-code-mcp-protocol)
;;; test-claude-code-mcp-protocol.el ends here
