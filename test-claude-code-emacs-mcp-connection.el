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

(provide 'test-claude-code-emacs-mcp-connection)
;;; test-claude-code-emacs-mcp-connection.el ends here
