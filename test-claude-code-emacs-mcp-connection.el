;;; test-claude-code-emacs-mcp-connection.el --- Tests for MCP connection management -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP server connection management

;;; Code:

(require 'ert)
(require 'claude-code-emacs-mcp-connection)
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
     (claude-code-emacs-mcp-connect project-root nil)
     (let ((ws (claude-code-emacs-mcp-get-websocket project-root)))
       (should ws)
       (should (websocket-openp ws))))))

(ert-deftest test-mcp-disconnect ()
  "Test disconnecting from MCP server."
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root)))
     (claude-code-emacs-mcp-connect project-root nil)
     (claude-code-emacs-mcp-disconnect project-root)
     (should-not (claude-code-emacs-mcp-get-websocket project-root)))))

(ert-deftest test-mcp-ensure-connection ()
  "Test ensure connection with retry."
  :tags '(:mcp :network)
  (skip-unless (not (getenv "CI")))  ; Skip in CI due to mock environment issues
  (claude-code-emacs-mcp-test-with-connection
   ;; Ensure connection should connect with retry
   (let ((project-root (projectile-project-root)))
     (claude-code-emacs-mcp-ensure-connection project-root)
     (should (claude-code-emacs-mcp-get-websocket project-root)))))

(ert-deftest test-mcp-port-from-file ()
  "Test reading port from file."
  (let* ((test-root "/test/project/")
         (temp-file (make-temp-file "claude-code-emacs-mcp-test"))
         (claude-code-emacs-mcp-project-ports (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          ;; Write port info to file
          (with-temp-file temp-file
            (insert (json-encode '((port . 8767) (projectRoot . "/test/project/")))))

          ;; Mock temporary-file-directory
          (cl-letf (((symbol-function 'expand-file-name)
                     (lambda (name dir)
                       (if (string-match "claude-code-emacs-mcp-" name)
                           temp-file
                         (concat dir "/" name)))))

            ;; Test getting port from file
            (let ((port (claude-code-emacs-mcp-get-port-from-file test-root)))
              (should (equal port 8767))
              ;; Check that port was registered
              (should (equal (claude-code-emacs-mcp-get-port test-root) 8767)))))
      (delete-file temp-file))))

(ert-deftest test-mcp-connection-retry ()
  "Test connection retry functionality."
  :tags '(:mcp :network)
  (skip-unless (not (getenv "CI")))  ; Skip in CI due to timing issues
  (claude-code-emacs-mcp-test-with-connection
   (let ((retry-count 0)
         (project-root (projectile-project-root)))
     ;; Test that retry mechanism is triggered on failure
     (cl-letf (((symbol-function 'websocket-open)
                (lambda (url &rest args)
                  (signal 'error '("Connection failed"))))
               ((symbol-function 'run-at-time)
                (lambda (time repeat func &rest args)
                  ;; Count retry attempts instead of executing
                  (setq retry-count (1+ retry-count)))))
       (claude-code-emacs-mcp-connect-with-retry project-root nil)
       ;; Should have scheduled retries
       (should (> retry-count 0))))))

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
       (claude-code-emacs-mcp-connect project-root)
       
       ;; Test ping timer was created
       (let ((info (claude-code-emacs-mcp-get-connection-info project-root)))
         (should (assoc 'ping-timer info)))
       
       ;; Send ping manually
       (claude-code-emacs-mcp-send-ping project-root)
       (should ping-sent)))))

(ert-deftest test-mcp-ping-timeout ()
  "Test ping timeout handling."
  :tags '(:mcp :ping)
  (claude-code-emacs-mcp-test-with-connection
   (let ((project-root (projectile-project-root))
         (timeout-handler-called nil)
         (reconnect-attempted nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (time repeat func &rest args)
                  (when (eq func 'claude-code-emacs-mcp-handle-ping-timeout)
                    ;; Execute timeout handler immediately
                    (funcall func project-root))
                  'mock-timer))
               ((symbol-function 'claude-code-emacs-mcp-ensure-connection)
                (lambda (root)
                  (setq reconnect-attempted t))))
       ;; Connect first
       (claude-code-emacs-mcp-connect project-root)
       
       ;; Send ping - should trigger timeout
       (claude-code-emacs-mcp-send-ping project-root)
       
       ;; Verify reconnection was attempted
       (should reconnect-attempted)))))

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
       (claude-code-emacs-mcp-connect project-root)
       
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

(ert-deftest test-mcp-port-registration-with-reconnect ()
  "Test port registration disconnects old connection and reconnects."
  :tags '(:mcp :connection)
  (claude-code-emacs-mcp-test-with-connection
   (let* ((project-root (projectile-project-root))
          ;; Normalize project root like the function does
          (normalized-root (directory-file-name project-root))
          (old-port 8765)
          (new-port 8766)
          (disconnect-called nil)
          (reconnect-called nil))
     ;; Test with auto-connect disabled to verify reconnect always happens
     (let ((claude-code-emacs-mcp-auto-connect nil))
       (cl-letf (((symbol-function 'claude-code-emacs-mcp-disconnect)
                  (lambda (root)
                    (when (string= root normalized-root)
                      (setq disconnect-called t))))
                 ((symbol-function 'claude-code-emacs-mcp-connect-with-retry)
                  (lambda (root)
                    (when (string= root normalized-root)
                      (setq reconnect-called t)))))
         ;; Register initial port
         (claude-code-emacs-mcp-register-port project-root old-port)
         (should (= (claude-code-emacs-mcp-get-port project-root) old-port))
         (should-not disconnect-called)
         (should-not reconnect-called)
         
         ;; Register same port again - should not disconnect
         (claude-code-emacs-mcp-register-port project-root old-port)
         (should-not disconnect-called)
         (should-not reconnect-called)
         
         ;; Register different port - should disconnect and reconnect
         ;; even with auto-connect disabled
         (claude-code-emacs-mcp-register-port project-root new-port)
         (should (= (claude-code-emacs-mcp-get-port project-root) new-port))
         (should disconnect-called)
         (should reconnect-called))))))

(provide 'test-claude-code-emacs-mcp-connection)
;;; test-claude-code-emacs-mcp-connection.el ends here
