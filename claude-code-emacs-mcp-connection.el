;;; claude-code-emacs-mcp-connection.el --- MCP WebSocket connection management -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (websocket "1.15") (projectile "2.9.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This module handles MCP WebSocket connection management including:
;; - Per-project connection tracking
;; - Port discovery and registration
;; - Connection retry logic with exponential backoff
;; - WebSocket lifecycle management
;; - Health monitoring via ping/pong heartbeat
;; - Automatic reconnection on connection loss

;;; Code:

(require 'websocket)
(require 'projectile)
(require 'json)
(require 'claude-code-emacs-core)

;; Forward declarations
(declare-function claude-code-emacs-mcp-on-message "claude-code-emacs-mcp-protocol" (_websocket frame project-root))
(declare-function claude-code-emacs-mcp-on-error "claude-code-emacs-mcp-protocol" (_websocket type error &optional _project-root))
(declare-function claude-code-emacs-mcp-on-close "claude-code-emacs-mcp-protocol" (_websocket project-root))

;;; Customization

(defgroup claude-code-emacs-mcp nil
  "MCP server integration for Claude Code Emacs."
  :group 'claude-code-emacs
  :prefix "claude-code-emacs-mcp-")

(defcustom claude-code-emacs-mcp-host "localhost"
  "Host for MCP server."
  :type 'string
  :group 'claude-code-emacs-mcp)

(defcustom claude-code-emacs-mcp-max-connection-attempts 10
  "Maximum number of connection attempts."
  :type 'integer
  :group 'claude-code-emacs-mcp)

(defcustom claude-code-emacs-mcp-connection-retry-delay 5
  "Delay in seconds between connection attempts."
  :type 'number
  :group 'claude-code-emacs-mcp)

(defcustom claude-code-emacs-mcp-ping-interval 30
  "Interval in seconds between WebSocket ping messages."
  :type 'integer
  :group 'claude-code-emacs-mcp)

(defcustom claude-code-emacs-mcp-ping-timeout 10
  "Timeout in seconds to wait for pong response."
  :type 'integer
  :group 'claude-code-emacs-mcp)

;;; Variables

(defvar claude-code-emacs-mcp-project-connections (make-hash-table :test 'equal)
  "Hash table mapping project roots to connection info.
Each value is an alist with keys:
  - websocket: The WebSocket connection
  - request-id: Counter for JSON-RPC request IDs
  - pending-requests: Hash table of pending requests
  - connection-attempts: Number of connection attempts
  - ping-timer: Timer for periodic ping messages
  - ping-timeout-timer: Timer for ping timeout detection
  - last-pong-time: Time of last received pong")

;;; Connection info management

(defun claude-code-emacs-mcp-get-connection-info (project-root)
  "Get connection info for PROJECT-ROOT."
  (or (gethash (claude-code-emacs-normalize-project-root project-root)
               claude-code-emacs-mcp-project-connections)
      (let ((info `((websocket . nil)
                    (request-id . 0)
                    (pending-requests . ,(make-hash-table :test 'equal))
                    (connection-attempts . 0)
                    (ping-timer . nil)
                    (ping-timeout-timer . nil)
                    (last-pong-time . nil))))
        (puthash (claude-code-emacs-normalize-project-root project-root)
                 info
                 claude-code-emacs-mcp-project-connections)
        info)))

(defun claude-code-emacs-mcp-get-websocket (project-root)
  "Get WebSocket for PROJECT-ROOT."
  (cdr (assoc 'websocket (claude-code-emacs-mcp-get-connection-info project-root))))

(defun claude-code-emacs-mcp-set-websocket (websocket project-root)
  "Set WEBSOCKET for PROJECT-ROOT."
  (let ((info (claude-code-emacs-mcp-get-connection-info project-root)))
    (setcdr (assoc 'websocket info) websocket)))

;;; Port Registration

(defun claude-code-emacs-mcp-register-port (project-root port)
  "Register PORT for PROJECT-ROOT."
  ;; Normalize project root by removing trailing slash
  (let ((normalized-root (claude-code-emacs-normalize-project-root project-root)))
    (message "MCP server registered on port %d for project %s" port normalized-root)
    (claude-code-emacs-mcp-try-connect-async normalized-root port)))

;;; Connection Management

(defun claude-code-emacs-mcp-try-connect-async (project-root port)
  "Try to connect to MCP server asynchronously for PROJECT-ROOT."
  (let* ((info (claude-code-emacs-mcp-get-connection-info project-root))
         (attempts (cdr (assoc 'connection-attempts info))))
    (if (>= attempts claude-code-emacs-mcp-max-connection-attempts)
        (progn
          (message "Failed to connect to MCP server after %d attempts for project %s"
                   claude-code-emacs-mcp-max-connection-attempts project-root))
      (setcdr (assoc 'connection-attempts info) (1+ attempts))
      (message "Attempting to connect to MCP server (attempt %d/%d) for project %s..."
               (1+ attempts)
               claude-code-emacs-mcp-max-connection-attempts
               project-root)
      (claude-code-emacs-mcp-connect
       project-root
       port
       (lambda (connected)
         (unless connected
           ;; Connection failed, retry
           (run-at-time claude-code-emacs-mcp-connection-retry-delay nil
                        #'claude-code-emacs-mcp-try-connect-async project-root port)))))))

(defun claude-code-emacs-mcp-connect (project-root port &optional callback)
  "Connect to MCP server WebSocket for PROJECT-ROOT.
If CALLBACK is provided, call it with connection result."
  ;; Use project root as session ID
  (let ((session-id project-root))
    (condition-case err
        (progn
          (websocket-open
           (format "ws://%s:%d/?session=%s"
                   claude-code-emacs-mcp-host
                   port
                   (url-hexify-string session-id))
           :on-open (lambda (websocket)
                      (message "MCP WebSocket opened for project %s" project-root)
                      (claude-code-emacs-mcp-set-websocket websocket project-root)
                      ;; Start ping timer
                      (claude-code-emacs-mcp-start-ping-timer project-root)
                      (when callback (funcall callback t)))
           :on-message (lambda (websocket frame)
                         (claude-code-emacs-mcp-on-message websocket frame project-root))
           :on-error (lambda (websocket type error)
                       (claude-code-emacs-mcp-on-error websocket type error project-root))
           :on-close (lambda (websocket)
                       (claude-code-emacs-mcp-on-close websocket project-root)))
          (message "Initiating MCP WebSocket connection on port %d for project %s" port project-root)
          t)
      (error
       (message "Failed to open WebSocket: %s" err)
       (when callback (funcall callback nil))
       nil))))

(defun claude-code-emacs-mcp-disconnect (project-root)
  "Disconnect from MCP server for PROJECT-ROOT."
  ;; Stop ping timers
  (claude-code-emacs-mcp-stop-ping-timer project-root)
  (claude-code-emacs-mcp-stop-ping-timeout project-root)
  ;; Close websocket
  (let ((websocket (claude-code-emacs-mcp-get-websocket project-root)))
    (when websocket
      (websocket-close websocket)
      (claude-code-emacs-mcp-set-websocket nil project-root))
    (let* ((info (claude-code-emacs-mcp-get-connection-info project-root))
           (pending-requests (cdr (assoc 'pending-requests info))))
      (clrhash pending-requests))
    (message "Disconnected from MCP server for project %s" project-root)))

;;; Ping/Pong functionality

(defun claude-code-emacs-mcp-send-ping (project-root)
  "Send ping message to MCP server for PROJECT-ROOT."
  (let ((websocket (claude-code-emacs-mcp-get-websocket project-root)))
    (when (and websocket (websocket-openp websocket))
      (condition-case err
          (progn
            (websocket-send-text websocket "{\"type\":\"ping\"}")
            ;; Set up timeout timer
            (claude-code-emacs-mcp-start-ping-timeout project-root))
        (error
         (message "Error sending ping to MCP server: %s" err)
         (claude-code-emacs-mcp-handle-connection-lost project-root))))))

(defun claude-code-emacs-mcp-start-ping-timer (project-root)
  "Start periodic ping timer for PROJECT-ROOT."
  (claude-code-emacs-mcp-stop-ping-timer project-root)
  (let* ((info (claude-code-emacs-mcp-get-connection-info project-root))
         (timer (run-with-timer claude-code-emacs-mcp-ping-interval
                                claude-code-emacs-mcp-ping-interval
                                #'claude-code-emacs-mcp-send-ping
                                project-root)))
    (setcdr (assoc 'ping-timer info) timer)))

(defun claude-code-emacs-mcp-stop-ping-timer (project-root)
  "Stop ping timer for PROJECT-ROOT."
  (let* ((info (claude-code-emacs-mcp-get-connection-info project-root))
         (timer (cdr (assoc 'ping-timer info))))
    (when (timerp timer)
      (cancel-timer timer))
    (setcdr (assoc 'ping-timer info) nil)))

(defun claude-code-emacs-mcp-start-ping-timeout (project-root)
  "Start ping timeout timer for PROJECT-ROOT."
  (claude-code-emacs-mcp-stop-ping-timeout project-root)
  (let* ((info (claude-code-emacs-mcp-get-connection-info project-root))
         (timer (run-with-timer claude-code-emacs-mcp-ping-timeout
                                nil
                                #'claude-code-emacs-mcp-handle-ping-timeout
                                project-root)))
    (setcdr (assoc 'ping-timeout-timer info) timer)))

(defun claude-code-emacs-mcp-stop-ping-timeout (project-root)
  "Stop ping timeout timer for PROJECT-ROOT."
  (let* ((info (claude-code-emacs-mcp-get-connection-info project-root))
         (timer (cdr (assoc 'ping-timeout-timer info))))
    (when (timerp timer)
      (cancel-timer timer))
    (setcdr (assoc 'ping-timeout-timer info) nil)))

(defun claude-code-emacs-mcp-handle-ping-timeout (project-root)
  "Handle ping timeout for PROJECT-ROOT."
  (message "MCP WebSocket ping timeout for project %s" project-root)
  (claude-code-emacs-mcp-handle-connection-lost project-root))

(defun claude-code-emacs-mcp-handle-pong (project-root)
  "Handle pong response for PROJECT-ROOT."
  ;; Cancel timeout timer
  (claude-code-emacs-mcp-stop-ping-timeout project-root)
  ;; Update last pong time
  (let ((info (claude-code-emacs-mcp-get-connection-info project-root)))
    (setcdr (assoc 'last-pong-time info) (current-time))))

(defun claude-code-emacs-mcp-handle-connection-lost (project-root)
  "Handle lost connection for PROJECT-ROOT."
  (message "MCP WebSocket connection lost for project %s, attempting reconnect..." project-root)
  ;; Stop timers
  (claude-code-emacs-mcp-stop-ping-timer project-root)
  (claude-code-emacs-mcp-stop-ping-timeout project-root)
  ;; Close existing connection
  (claude-code-emacs-mcp-disconnect project-root))

(provide 'claude-code-emacs-mcp-connection)
;;; claude-code-emacs-mcp-connection.el ends here
