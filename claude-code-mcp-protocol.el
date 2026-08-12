;;; claude-code-mcp-protocol.el --- JSON-RPC protocol implementation for MCP -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0

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

;; This module implements the JSON-RPC protocol for MCP communication:
;; - Message parsing and dispatching
;; - Request/response handling
;; - Error handling
;; - WebSocket event handlers
;;
;; All messages are routed per server instance ID so that multiple
;; Claude Code sessions in the same project never receive each other's
;; responses.

;;; Code:

(require 'json)
(require 'websocket nil t)
(require 'projectile)

;; Declare websocket functions to avoid eager macro-expansion failures
(declare-function websocket-send-text "websocket" (websocket text))
(declare-function websocket-frame-text "websocket" (frame))

;; Forward declarations
(declare-function claude-code-mcp-get-connection-info "claude-code-mcp-connection" (instance-id))
(declare-function claude-code-mcp-get-websocket "claude-code-mcp-connection" (instance-id))
(declare-function claude-code-mcp-disconnect "claude-code-mcp-connection" (instance-id))
(declare-function claude-code-mcp-handle-pong "claude-code-mcp-connection" (instance-id))

;; Tool handler forward declarations
(declare-function claude-code-mcp-handle-getOpenBuffers "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-getCurrentSelection "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-getDiagnostics "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-get-buffer-content "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-get-project-info "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-get-project-files "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-getDefinition "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-findReferences "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-describeSymbol "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-openDiffFile "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-openRevisionDiff "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-openCurrentChanges "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-openDiffContent "claude-code-mcp-tools" (params))
(declare-function claude-code-mcp-handle-sendNotification "claude-code-mcp-tools" (params))

;;; JSON-RPC Communication

(defun claude-code-mcp-send-response (id result error instance-id)
  "Send response for request ID with RESULT or ERROR to INSTANCE-ID.
The response goes to the websocket of the instance the request came
from, never to another instance of the same project."
  (let ((websocket (claude-code-mcp-get-websocket instance-id))
        (response (if error
                      `((jsonrpc . "2.0")
                        (id . ,id)
                        (error . ,error))
                    `((jsonrpc . "2.0")
                      (id . ,id)
                      (result . ,result)))))
    (when websocket
      (websocket-send-text websocket (json-encode response)))))

;;; Message Handling

(defun claude-code-mcp-handle-message (message instance-id)
  "Handle incoming JSON-RPC MESSAGE from INSTANCE-ID."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (msg (json-read-from-string message)))
        (cond
         ;; Handle ping/pong messages
         ((equal (cdr (assoc 'type msg)) "pong")
          (claude-code-mcp-handle-pong instance-id))

         ;; Request from server (check method first)
         ((assoc 'method msg)
          (claude-code-mcp-handle-request msg instance-id))

         ;; Response to our request
         ((assoc 'id msg)
          (when-let* ((id (cdr (assoc 'id msg)))
                      (info (claude-code-mcp-get-connection-info instance-id))
                      (pending-requests (cdr (assoc 'pending-requests info)))
                      (callback (gethash id pending-requests)))
            (remhash id pending-requests)
            (if (assoc 'error msg)
                (funcall callback nil (cdr (assoc 'error msg)))
              (funcall callback (cdr (assoc 'result msg)) nil))))

         ;; Invalid message
         (t
          (message "Invalid JSON-RPC message: %s" message))))
    (error
     (message "Error handling MCP message: %s" err))))

(defun claude-code-mcp-handle-request (request instance-id)
  "Handle incoming REQUEST from MCP server INSTANCE-ID."
  (let* ((id (cdr (assoc 'id request)))
         (method (cdr (assoc 'method request)))
         (params (cdr (assoc 'params request)))
         (handler (intern (format "claude-code-mcp-handle-%s" method))))

    (message "MCP Request: method=%s, handler=%s, fboundp=%s"
             method handler (fboundp handler))

    (if (fboundp handler)
        (condition-case err
            (let ((result (funcall handler params)))
              (claude-code-mcp-send-response id result nil instance-id))
          (error
           (message "Error in handler %s: %s" handler err)
           (claude-code-mcp-send-response id nil
                                          `((code . -32603)
                                            (message . ,(error-message-string err)))
                                          instance-id)))
      (claude-code-mcp-send-response id nil
                                     `((code . -32601)
                                       (message . ,(format "Method not found: %s" method)))
                                     instance-id))))

;;; WebSocket Event Handlers

(defun claude-code-mcp-on-message (_websocket frame instance-id)
  "Handle incoming WebSocket message from INSTANCE-ID."
  (let ((payload (websocket-frame-text frame)))
    (when payload
      (claude-code-mcp-handle-message payload instance-id))))

(defun claude-code-mcp-on-error (_websocket type error &optional _instance-id)
  "Handle WebSocket error."
  (message "MCP WebSocket error (%s): %s" type error))

(defun claude-code-mcp-on-close (websocket instance-id)
  "Handle WebSocket close of WEBSOCKET for INSTANCE-ID.
Cleans up the instance only when the closed socket is its current
connection.  A stale socket (replaced by a reconnect) closing late
must not tear down the live connection."
  (when-let ((info (claude-code-mcp-get-connection-info instance-id)))
    (when (eq websocket (cdr (assoc 'websocket info)))
      (message "MCP WebSocket connection closed for instance %s" instance-id)
      (claude-code-mcp-disconnect instance-id))))

(provide 'claude-code-mcp-protocol)
;;; claude-code-mcp-protocol.el ends here
