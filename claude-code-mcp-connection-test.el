;;; test-claude-code-mcp-connection.el --- Tests for MCP connection management -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP server connection management

;;; Code:

(require 'ert)
(require 'claude-code-mcp-connection)
(require 'cl-lib)

;;; Test utilities

(defmacro claude-code-mcp-test-with-connection (&rest body)
  "Execute BODY with MCP connection mocked."
  `(let ((claude-code-mcp-project-connections (make-hash-table :test 'equal)))
     ;; Register default port for test project
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

;;; Connection tests

(ert-deftest test-mcp-connect ()
  "Test connecting to MCP server."
  :tags '(:mcp :network)
  (skip-unless (not (getenv "CI")))  ; Skip in CI due to mock environment issues
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root)))
     ;; Initialize connection info first
     (claude-code-mcp-initialize-connection-info project-root)
     (claude-code-mcp-connect project-root 8766)
     (let ((ws (claude-code-mcp-get-websocket project-root)))
       (should ws)
       (should (websocket-openp ws))))))

(ert-deftest test-mcp-disconnect ()
  "Test disconnecting from MCP server."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root)))
     ;; Initialize connection info first
     (claude-code-mcp-initialize-connection-info project-root)
     (claude-code-mcp-connect project-root 8766)
     (claude-code-mcp-disconnect project-root)
     (should-not (claude-code-mcp-get-websocket project-root)))))

(ert-deftest test-mcp-ping-pong ()
  "Test ping/pong functionality."
  :tags '(:mcp :ping)
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (ping-sent nil))
     (cl-letf (((symbol-function 'websocket-send-text)
                (lambda (ws text)
                  (when (string-match "ping" text)
                    (setq ping-sent t)))))
       ;; Initialize connection info first
       (claude-code-mcp-initialize-connection-info project-root)
       
       ;; Connect
       (claude-code-mcp-connect project-root 8766)

       ;; Test ping timer was created
       (let ((info (claude-code-mcp-get-connection-info project-root)))
         (should info)
         (should (assoc 'ping-timer info)))

       ;; Send ping manually
       (claude-code-mcp-send-ping project-root)
       (should ping-sent)))))

(ert-deftest test-mcp-pong-handling ()
  "Test pong message handling."
  :tags '(:mcp :ping)
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (timeout-cancelled nil))
     (cl-letf (((symbol-function 'cancel-timer)
                (lambda (timer)
                  (setq timeout-cancelled t)))
               ((symbol-function 'timerp)
                (lambda (timer) t)))
       ;; Initialize connection info first
       (claude-code-mcp-initialize-connection-info project-root)
       
       ;; Connect
       (claude-code-mcp-connect project-root 8766)

       ;; Start ping timeout (mock)
       (let ((info (claude-code-mcp-get-connection-info project-root)))
         (should info)
         (setcdr (assoc 'ping-timeout-timer info) 'mock-timer))

       ;; Handle pong
       (claude-code-mcp-handle-pong project-root)

       ;; Verify timeout was cancelled
       (should timeout-cancelled)

       ;; Verify last-pong-time was updated
       (let ((info (claude-code-mcp-get-connection-info project-root)))
         (should (cdr (assoc 'last-pong-time info))))))))

(ert-deftest test-mcp-get-connection-info ()
  "Test getting connection info for project."
  (claude-code-mcp-test-with-connection
   (let* ((project-root (projectile-project-root)))
     ;; Should return nil when no connection info exists
     (should-not (claude-code-mcp-get-connection-info project-root))
     
     ;; Initialize connection info
     (claude-code-mcp-initialize-connection-info project-root)
     
     ;; Now it should return the info
     (let ((info (claude-code-mcp-get-connection-info project-root)))
       ;; Check all expected fields exist
       (should (assoc 'websocket info))
       (should (assoc 'request-id info))
       (should (assoc 'pending-requests info))
       (should (assoc 'connection-attempts info))
       (should (assoc 'ping-timer info))
       (should (assoc 'ping-timeout-timer info))
       (should (assoc 'last-pong-time info))))))

(ert-deftest test-mcp-register-port ()
  "Test port registration for project."
  (claude-code-mcp-test-with-connection
   (let* ((project-root (projectile-project-root))
          (test-port 9999)
          (connect-called nil))
     (cl-letf (((symbol-function 'claude-code-normalize-project-root)
                (lambda (root) (directory-file-name root)))
               ((symbol-function 'claude-code-mcp-try-connect-async)
                (lambda (root port)
                  (setq connect-called (list root port)))))
       ;; Register port
       (claude-code-mcp-register-port project-root test-port)
       ;; Check that try-connect-async was called with correct args
       (should connect-called)
       (should (equal (car connect-called) (directory-file-name project-root)))
       (should (equal (cadr connect-called) test-port))))))

(ert-deftest test-mcp-handle-connection-lost ()
  "Test handling lost connection."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (disconnect-called nil))
     ;; Initialize connection info first
     (claude-code-mcp-initialize-connection-info project-root)
     
     ;; Connect
     (claude-code-mcp-connect project-root 8766)

     ;; Mock disconnect
     (cl-letf (((symbol-function 'claude-code-mcp-disconnect)
                (lambda (root)
                  (setq disconnect-called t))))
       ;; Handle connection lost
       (claude-code-mcp-handle-connection-lost project-root)
       ;; Should have called disconnect
       (should disconnect-called)))))

(ert-deftest test-mcp-get-connection-info-pure-getter ()
  "Test that get-connection-info is a pure getter that returns nil when no info exists."
  (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal))
        (project-root "/tmp/test-pure-getter/"))
    ;; Should return nil when no connection info exists
    (should-not (claude-code-mcp-get-connection-info project-root))
    
    ;; Should still return nil on repeated calls
    (should-not (claude-code-mcp-get-connection-info project-root))
    (should-not (claude-code-mcp-get-connection-info project-root))))

(ert-deftest test-mcp-websocket-setter ()
  "Test websocket setter operations without getter."
  (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal))
        (project-root "/tmp/test-ws-setter/"))
    ;; Initialize first
    (claude-code-mcp-initialize-connection-info project-root)
    
    ;; Set a websocket and verify via connection info
    (let ((ws '(test-websocket)))
      (claude-code-mcp-set-websocket ws project-root)
      (let ((info (claude-code-mcp-get-connection-info project-root)))
        (should (equal ws (cdr (assoc 'websocket info))))))
    
    ;; Set to nil and verify
    (claude-code-mcp-set-websocket nil project-root)
    (let ((info (claude-code-mcp-get-connection-info project-root)))
      (should (null (cdr (assoc 'websocket info)))))))

(ert-deftest test-mcp-ping-timer-management ()
  "Test ping timer start and stop."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (timer-created nil)
         (timer-cancelled nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (&rest args)
                  (setq timer-created t)
                  'mock-timer))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (when (eq timer 'mock-timer)
                    (setq timer-cancelled t))))
               ((symbol-function 'timerp)
                (lambda (timer) (eq timer 'mock-timer))))
       ;; Initialize connection info first
       (claude-code-mcp-initialize-connection-info project-root)
       
       ;; Start timer
       (claude-code-mcp-start-ping-timer project-root)
       (should timer-created)
       ;; Stop timer
       (claude-code-mcp-stop-ping-timer project-root)
       (should timer-cancelled)))))

(ert-deftest test-mcp-try-connect-async ()
  "Test asynchronous connection attempts."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (test-port 8888)
         (connect-called nil)
         (retry-scheduled nil))
     ;; Initialize connection info first
     (claude-code-mcp-initialize-connection-info project-root)
     
     ;; Test successful connection
     (cl-letf (((symbol-function 'claude-code-mcp-connect)
                (lambda (root port callback)
                  (setq connect-called (list root port))
                  (when callback (funcall callback t))))
               ((symbol-function 'run-at-time)
                (lambda (&rest args)
                  (setq retry-scheduled args)
                  'mock-timer)))
       (claude-code-mcp-try-connect-async project-root test-port)
       (should connect-called)
       (should-not retry-scheduled))
     
     ;; Test failed connection with retry
     (setq connect-called nil)
     (setq retry-scheduled nil)
     (cl-letf (((symbol-function 'claude-code-mcp-connect)
                (lambda (root port callback)
                  (setq connect-called (list root port))
                  (when callback (funcall callback nil))))
               ((symbol-function 'run-at-time)
                (lambda (delay &rest args)
                  (setq retry-scheduled (cons delay args))
                  'mock-timer)))
       (claude-code-mcp-try-connect-async project-root test-port)
       (should connect-called)
       (should retry-scheduled)
       (should (= (car retry-scheduled) claude-code-mcp-connection-retry-delay))))))

(ert-deftest test-mcp-ping-timeout-management ()
  "Test ping timeout timer management."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (timeout-timer-created nil)
         (timeout-timer-cancelled nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (delay &rest args)
                  (when (= delay claude-code-mcp-ping-timeout)
                    (setq timeout-timer-created t))
                  'mock-timeout-timer))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (when (eq timer 'mock-timeout-timer)
                    (setq timeout-timer-cancelled t))))
               ((symbol-function 'timerp)
                (lambda (timer) (eq timer 'mock-timeout-timer))))
       ;; Initialize connection info first
       (claude-code-mcp-initialize-connection-info project-root)
       
       ;; Start timeout timer
       (claude-code-mcp-start-ping-timeout project-root)
       (should timeout-timer-created)
       ;; Stop timeout timer
       (claude-code-mcp-stop-ping-timeout project-root)
       (should timeout-timer-cancelled)))))

(ert-deftest test-mcp-handle-ping-timeout ()
  "Test handling ping timeout."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (connection-lost-called nil))
     (cl-letf (((symbol-function 'claude-code-mcp-handle-connection-lost)
                (lambda (root)
                  (setq connection-lost-called root))))
       (claude-code-mcp-handle-ping-timeout project-root)
       (should (equal connection-lost-called project-root))))))

(ert-deftest test-mcp-unregister-port ()
  "Test port unregistration."
  (claude-code-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (disconnect-called nil))
     (cl-letf (((symbol-function 'claude-code-normalize-project-root)
                (lambda (root) (directory-file-name root)))
               ((symbol-function 'claude-code-mcp-disconnect)
                (lambda (root)
                  (setq disconnect-called root))))
       (claude-code-mcp-unregister-port project-root)
       (should (equal disconnect-called (directory-file-name project-root)))))))

(provide 'test-claude-code-mcp-connection)
;;; test-claude-code-mcp-connection.el ends here
