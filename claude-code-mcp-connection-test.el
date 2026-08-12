;;; test-claude-code-mcp-connection.el --- Tests for MCP connection management -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the MCP server connection management.
;; Connections are keyed by server instance ID so that multiple Claude
;; Code sessions (agents) in the same project can coexist.

;;; Code:

(require 'ert)
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
                     ;; Call on-open callback immediately
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

;;; Connection info tests

(ert-deftest test-mcp-initialize-connection-info ()
  "Test that connection info is keyed by instance ID."
  (claude-code-mcp-test-with-connection
   ;; Should return nil when no connection info exists
   (should-not (claude-code-mcp-get-connection-info "inst-a"))

   (claude-code-mcp-initialize-connection-info "inst-a" "/tmp/proj/" 1111)

   (let ((info (claude-code-mcp-get-connection-info "inst-a")))
     (should info)
     ;; Project root is normalized (no trailing slash)
     (should (equal (cdr (assoc 'project-root info)) "/tmp/proj"))
     (should (equal (cdr (assoc 'port info)) 1111))
     ;; Check all expected fields exist
     (should (assoc 'websocket info))
     (should (assoc 'request-id info))
     (should (assoc 'pending-requests info))
     (should (assoc 'connection-attempts info))
     (should (assoc 'ping-timer info))
     (should (assoc 'ping-timeout-timer info))
     (should (assoc 'last-pong-time info)))))

(ert-deftest test-mcp-register-port-creates-instance-entry ()
  "Test that registering a port creates an instance-keyed connection."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj/" 1111 "inst-a")
   (let ((info (claude-code-mcp-get-connection-info "inst-a")))
     (should info)
     (should (equal (cdr (assoc 'project-root info)) "/tmp/proj"))
     (should (equal (cdr (assoc 'port info)) 1111))
     ;; Mock websocket-open connects immediately
     (should (websocket-openp (claude-code-mcp-get-websocket "inst-a"))))))

(ert-deftest test-mcp-multiple-instances-same-project ()
  "Two instances for the same project must coexist.
Registering a second instance must not touch the first instance's
connection."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (let ((ws-a (claude-code-mcp-get-websocket "inst-a")))
     (claude-code-mcp-register-port "/tmp/proj" 2222 "inst-b")
     (let ((ws-b (claude-code-mcp-get-websocket "inst-b")))
       ;; Both connections exist and are distinct
       (should ws-a)
       (should ws-b)
       (should-not (eq ws-a ws-b))
       ;; First instance is still connected and its websocket unchanged
       (should (eq ws-a (claude-code-mcp-get-websocket "inst-a")))
       (should (websocket-openp ws-a))
       (should (websocket-openp ws-b))))))

(ert-deftest test-mcp-register-same-instance-id-replaces ()
  "Re-registering the same instance ID replaces its connection."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (let ((old-ws (claude-code-mcp-get-websocket "inst-a")))
     (claude-code-mcp-register-port "/tmp/proj" 3333 "inst-a")
     (let ((new-ws (claude-code-mcp-get-websocket "inst-a")))
       ;; Old socket was closed, new one is live
       (should (memq old-ws claude-code-mcp-test-closed-sockets))
       (should (websocket-openp new-ws))
       (should (equal (cdr (assoc 'port (claude-code-mcp-get-connection-info "inst-a")))
                      3333))))))

(ert-deftest test-mcp-unregister-removes-only-own-instance ()
  "Unregistering one instance must not affect other instances."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (claude-code-mcp-register-port "/tmp/proj" 2222 "inst-b")
   (let ((ws-a (claude-code-mcp-get-websocket "inst-a"))
         (ws-b (claude-code-mcp-get-websocket "inst-b")))
     (claude-code-mcp-unregister-port "inst-a")
     ;; inst-a is fully removed
     (should-not (claude-code-mcp-get-connection-info "inst-a"))
     (should (memq ws-a claude-code-mcp-test-closed-sockets))
     ;; inst-b survives untouched
     (should (claude-code-mcp-get-connection-info "inst-b"))
     (should (websocket-openp ws-b)))))

(ert-deftest test-mcp-disconnect-removes-instance ()
  "Disconnect cleans up and removes the instance entry."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (let ((ws (claude-code-mcp-get-websocket "inst-a")))
     (claude-code-mcp-disconnect "inst-a")
     (should (memq ws claude-code-mcp-test-closed-sockets))
     (should-not (claude-code-mcp-get-connection-info "inst-a")))))

(ert-deftest test-mcp-connection-lost-removes-only-instance ()
  "Connection loss on one instance must not affect other instances."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (claude-code-mcp-register-port "/tmp/proj" 2222 "inst-b")
   (claude-code-mcp-handle-connection-lost "inst-a")
   (should-not (claude-code-mcp-get-connection-info "inst-a"))
   (should (claude-code-mcp-get-connection-info "inst-b"))
   (should (websocket-openp (claude-code-mcp-get-websocket "inst-b")))))

;;; Event broadcast tests

(ert-deftest test-mcp-send-event-broadcasts-to-all-project-instances ()
  "Events for a project are sent to every instance of that project only."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (claude-code-mcp-register-port "/tmp/proj" 2222 "inst-b")
   (claude-code-mcp-register-port "/tmp/other" 3333 "inst-c")
   ;; Reset messages sent during connection setup
   (setq claude-code-mcp-test-sent-messages nil)

   (claude-code-mcp-send-event-to-project "/tmp/proj" "testEvent" '((foo . "bar")))

   (let ((msgs-a (claude-code-mcp-test-messages-for (claude-code-mcp-get-websocket "inst-a")))
         (msgs-b (claude-code-mcp-test-messages-for (claude-code-mcp-get-websocket "inst-b")))
         (msgs-c (claude-code-mcp-test-messages-for (claude-code-mcp-get-websocket "inst-c"))))
     ;; Both instances of /tmp/proj receive the event
     (should (= 1 (length msgs-a)))
     (should (= 1 (length msgs-b)))
     (should (string-match-p "emacs/testEvent" (car msgs-a)))
     (should (string-match-p "emacs/testEvent" (car msgs-b)))
     ;; The other project receives nothing
     (should (= 0 (length msgs-c))))))

(ert-deftest test-mcp-send-event-accepts-trailing-slash-root ()
  "Event root is normalized before matching instances."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (setq claude-code-mcp-test-sent-messages nil)
   (claude-code-mcp-send-event-to-project "/tmp/proj/" "testEvent" '((foo . "bar")))
   (should (= 1 (length claude-code-mcp-test-sent-messages)))))

;;; Ping/pong tests

(ert-deftest test-mcp-ping-sent-to-own-instance-socket ()
  "Ping goes to the instance's own websocket."
  (claude-code-mcp-test-with-connection
   (claude-code-mcp-register-port "/tmp/proj" 1111 "inst-a")
   (claude-code-mcp-register-port "/tmp/proj" 2222 "inst-b")
   (setq claude-code-mcp-test-sent-messages nil)
   (cl-letf (((symbol-function 'run-with-timer) (lambda (&rest _args) nil)))
     (claude-code-mcp-send-ping "inst-a"))
   (let ((msgs-a (claude-code-mcp-test-messages-for (claude-code-mcp-get-websocket "inst-a")))
         (msgs-b (claude-code-mcp-test-messages-for (claude-code-mcp-get-websocket "inst-b"))))
     (should (= 1 (length msgs-a)))
     (should (string-match-p "ping" (car msgs-a)))
     (should (= 0 (length msgs-b))))))

(ert-deftest test-mcp-ping-timer-management ()
  "Test ping timer start and stop per instance."
  (claude-code-mcp-test-with-connection
   (let ((timer-created nil)
         (timer-cancelled nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (&rest _args)
                  (setq timer-created t)
                  'mock-timer))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (when (eq timer 'mock-timer)
                    (setq timer-cancelled t))))
               ((symbol-function 'timerp)
                (lambda (timer) (eq timer 'mock-timer))))
       (claude-code-mcp-initialize-connection-info "inst-a" "/tmp/proj" 1111)
       (claude-code-mcp-start-ping-timer "inst-a")
       (should timer-created)
       (claude-code-mcp-stop-ping-timer "inst-a")
       (should timer-cancelled)))))

(ert-deftest test-mcp-ping-timeout-management ()
  "Test ping timeout timer management per instance."
  (claude-code-mcp-test-with-connection
   (let ((timeout-timer-created nil)
         (timeout-timer-cancelled nil))
     (cl-letf (((symbol-function 'run-with-timer)
                (lambda (delay &rest _args)
                  (when (= delay claude-code-mcp-ping-timeout)
                    (setq timeout-timer-created t))
                  'mock-timeout-timer))
               ((symbol-function 'cancel-timer)
                (lambda (timer)
                  (when (eq timer 'mock-timeout-timer)
                    (setq timeout-timer-cancelled t))))
               ((symbol-function 'timerp)
                (lambda (timer) (eq timer 'mock-timeout-timer))))
       (claude-code-mcp-initialize-connection-info "inst-a" "/tmp/proj" 1111)
       (claude-code-mcp-start-ping-timeout "inst-a")
       (should timeout-timer-created)
       (claude-code-mcp-stop-ping-timeout "inst-a")
       (should timeout-timer-cancelled)))))

(ert-deftest test-mcp-handle-pong-per-instance ()
  "Pong cancels the timeout of its own instance only."
  (claude-code-mcp-test-with-connection
   (let ((cancelled nil))
     (cl-letf (((symbol-function 'cancel-timer)
                (lambda (timer) (push timer cancelled)))
               ((symbol-function 'timerp)
                (lambda (timer) (memq timer '(timer-a timer-b)))))
       (claude-code-mcp-initialize-connection-info "inst-a" "/tmp/proj" 1111)
       (claude-code-mcp-initialize-connection-info "inst-b" "/tmp/proj" 2222)
       (setcdr (assoc 'ping-timeout-timer (claude-code-mcp-get-connection-info "inst-a"))
               'timer-a)
       (setcdr (assoc 'ping-timeout-timer (claude-code-mcp-get-connection-info "inst-b"))
               'timer-b)

       (claude-code-mcp-handle-pong "inst-a")

       ;; Only inst-a's timer was cancelled
       (should (equal cancelled '(timer-a)))
       ;; inst-a's last-pong-time was updated, inst-b's was not
       (should (cdr (assoc 'last-pong-time (claude-code-mcp-get-connection-info "inst-a"))))
       (should-not (cdr (assoc 'last-pong-time (claude-code-mcp-get-connection-info "inst-b"))))
       ;; inst-b's timeout timer is still set
       (should (eq (cdr (assoc 'ping-timeout-timer (claude-code-mcp-get-connection-info "inst-b")))
                   'timer-b))))))

(ert-deftest test-mcp-handle-ping-timeout ()
  "Ping timeout triggers connection-lost handling for the instance."
  (claude-code-mcp-test-with-connection
   (let ((connection-lost-called nil))
     (cl-letf (((symbol-function 'claude-code-mcp-handle-connection-lost)
                (lambda (instance-id)
                  (setq connection-lost-called instance-id))))
       (claude-code-mcp-handle-ping-timeout "inst-a")
       (should (equal connection-lost-called "inst-a"))))))

;;; Retry tests

(ert-deftest test-mcp-try-connect-async ()
  "Test asynchronous connection attempts with retry."
  (claude-code-mcp-test-with-connection
   (let ((connect-called nil)
         (retry-scheduled nil))
     (claude-code-mcp-initialize-connection-info "inst-a" "/tmp/proj" 8888)

     ;; Successful connection schedules no retry
     (cl-letf (((symbol-function 'claude-code-mcp-connect)
                (lambda (instance-id &optional callback)
                  (setq connect-called instance-id)
                  (when callback (funcall callback t))))
               ((symbol-function 'run-at-time)
                (lambda (&rest args)
                  (setq retry-scheduled args)
                  'mock-timer)))
       (claude-code-mcp-try-connect-async "inst-a")
       (should (equal connect-called "inst-a"))
       (should-not retry-scheduled))

     ;; Failed connection schedules a retry
     (setq connect-called nil)
     (setq retry-scheduled nil)
     (cl-letf (((symbol-function 'claude-code-mcp-connect)
                (lambda (instance-id &optional callback)
                  (setq connect-called instance-id)
                  (when callback (funcall callback nil))))
               ((symbol-function 'run-at-time)
                (lambda (delay &rest args)
                  (setq retry-scheduled (cons delay args))
                  'mock-timer)))
       (claude-code-mcp-try-connect-async "inst-a")
       (should (equal connect-called "inst-a"))
       (should retry-scheduled)
       (should (= (car retry-scheduled) claude-code-mcp-connection-retry-delay))))))

(ert-deftest test-mcp-try-connect-async-aborts-after-unregister ()
  "Retry attempts stop once the instance has been unregistered."
  (claude-code-mcp-test-with-connection
   (let ((connect-called nil))
     (cl-letf (((symbol-function 'claude-code-mcp-connect)
                (lambda (instance-id &optional _callback)
                  (setq connect-called instance-id))))
       ;; No connection info registered for this instance
       (claude-code-mcp-try-connect-async "inst-gone")
       (should-not connect-called)))))

(provide 'test-claude-code-mcp-connection)
;;; test-claude-code-mcp-connection.el ends here
