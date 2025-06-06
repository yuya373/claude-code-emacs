;;; test-claude-code-emacs-mcp-connection.el --- Tests for MCP connection management -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP server connection management

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-connection)
(require 'cl-lib)

;;; Test utilities

(defmacro claude-code-emacs-mcp-test-with-connection (&rest body)
  "Execute BODY with MCP connection mocked."
  `(let ((claude-code-emacs-mcp-project-connections (make-hash-table :test 'equal)))
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
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root)))
     (claude-code-emacs-mcp-connect project-root 8766)
     (let ((ws (claude-code-emacs-mcp-get-websocket project-root)))
       (should ws)
       (should (websocket-openp ws))))))

(ert-deftest test-mcp-disconnect ()
  "Test disconnecting from MCP server."
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root)))
     (claude-code-emacs-mcp-connect project-root 8766)
     (claude-code-emacs-mcp-disconnect project-root)
     (should-not (claude-code-emacs-mcp-get-websocket project-root)))))

(ert-deftest test-mcp-ping-pong ()
  "Test ping/pong functionality."
  :tags '(:mcp :ping)
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (ping-sent nil))
     (cl-letf (((symbol-function 'websocket-send-text)
                (lambda (ws text)
                  (when (string-match "ping" text)
                    (setq ping-sent t)))))
       ;; Connect first
       (claude-code-emacs-mcp-connect project-root 8766)

       ;; Test ping timer was created
       (let ((info (claude-code-emacs-mcp-get-connection-info project-root)))
         (should (assoc 'ping-timer info)))

       ;; Send ping manually
       (claude-code-emacs-mcp-send-ping project-root)
       (should ping-sent)))))

(ert-deftest test-mcp-pong-handling ()
  "Test pong message handling."
  :tags '(:mcp :ping)
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (timeout-cancelled nil))
     (cl-letf (((symbol-function 'cancel-timer)
                (lambda (timer)
                  (setq timeout-cancelled t)))
               ((symbol-function 'timerp)
                (lambda (timer) t)))
       ;; Connect first
       (claude-code-emacs-mcp-connect project-root 8766)

       ;; Start ping timeout (mock)
       (let ((info (claude-code-emacs-mcp-get-connection-info project-root)))
         (setcdr (assoc 'ping-timeout-timer info) 'mock-timer))

       ;; Handle pong
       (claude-code-emacs-mcp-handle-pong project-root)

       ;; Verify timeout was cancelled
       (should timeout-cancelled)

       ;; Verify last-pong-time was updated
       (let ((info (claude-code-emacs-mcp-get-connection-info project-root)))
         (should (cdr (assoc 'last-pong-time info))))))))

(ert-deftest test-mcp-get-connection-info ()
  "Test getting connection info for project."
  (claude-code-emacs-mcp-test-with-connection
   (let* ((project-root (projectile-project-root))
          (info (claude-code-emacs-mcp-get-connection-info project-root)))
     ;; Check all expected fields exist
     (should (assoc 'websocket info))
     (should (assoc 'request-id info))
     (should (assoc 'pending-requests info))
     (should (assoc 'connection-attempts info))
     (should (assoc 'ping-timer info))
     (should (assoc 'ping-timeout-timer info))
     (should (assoc 'last-pong-time info)))))

(ert-deftest test-mcp-register-port ()
  "Test port registration for project."
  (claude-code-emacs-mcp-test-with-connection
   (let* ((project-root (projectile-project-root))
          (test-port 9999)
          (connect-called nil))
     (cl-letf (((symbol-function 'claude-code-emacs-normalize-project-root)
                (lambda (root) (directory-file-name root)))
               ((symbol-function 'claude-code-emacs-mcp-try-connect-async)
                (lambda (root port)
                  (setq connect-called (list root port)))))
       ;; Register port
       (claude-code-emacs-mcp-register-port project-root test-port)
       ;; Check that try-connect-async was called with correct args
       (should connect-called)
       (should (equal (car connect-called) (directory-file-name project-root)))
       (should (equal (cadr connect-called) test-port))))))

(ert-deftest test-mcp-handle-connection-lost ()
  "Test handling lost connection."
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (disconnect-called nil))
     ;; Connect first
     (claude-code-emacs-mcp-connect project-root 8766)

     ;; Mock disconnect
     (cl-letf (((symbol-function 'claude-code-emacs-mcp-disconnect)
                (lambda (root)
                  (setq disconnect-called t))))
       ;; Handle connection lost
       (claude-code-emacs-mcp-handle-connection-lost project-root)
       ;; Should have called disconnect
       (should disconnect-called)))))

(ert-deftest test-mcp-set-and-get-websocket ()
  "Test setting and getting websocket."
  (claude-code-emacs-mcp-test-with-connection
   (let* ((project-root (projectile-project-root))
          (new-ws '(new-mock-websocket)))
     ;; Get the initial websocket (may be nil or a default)
     (let ((initial-ws (claude-code-emacs-mcp-get-websocket project-root)))
       ;; Set a new websocket
       (claude-code-emacs-mcp-set-websocket new-ws project-root)
       ;; Should get the new websocket back
       (should (equal (claude-code-emacs-mcp-get-websocket project-root) new-ws))
       ;; Should not be the initial websocket
       (should-not (equal (claude-code-emacs-mcp-get-websocket project-root) initial-ws))))))

(ert-deftest test-mcp-ping-timer-management ()
  "Test ping timer start and stop."
  (claude-code-emacs-mcp-test-with-connection
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
       ;; Start timer
       (claude-code-emacs-mcp-start-ping-timer project-root)
       (should timer-created)
       ;; Stop timer
       (claude-code-emacs-mcp-stop-ping-timer project-root)
       (should timer-cancelled)))))

(provide 'test-claude-code-emacs-mcp-connection)
;;; test-claude-code-emacs-mcp-connection.el ends here
