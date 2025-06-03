;;; claude-code-emacs-mcp.el --- MCP server integration for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (projectile "2.9.1") (lsp-mode "9.0.0") (json-rpc "0.0.2"))

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

;; MCP (Model Context Protocol) server integration for Claude Code Emacs.
;; This module provides Emacs functionality access to Claude Code through MCP.

;;; Code:

(require 'json)
(require 'projectile)
(require 'lsp-mode nil t)
(require 'lsp-protocol nil t)

;;; Customization

(defgroup claude-code-emacs-mcp nil
  "MCP server integration for Claude Code Emacs."
  :group 'claude-code-emacs
  :prefix "claude-code-emacs-mcp-")

(defcustom claude-code-emacs-mcp-port 8766
  "Port number for MCP server."
  :type 'integer
  :group 'claude-code-emacs-mcp)

(defcustom claude-code-emacs-mcp-host "localhost"
  "Host for MCP server."
  :type 'string
  :group 'claude-code-emacs-mcp)


;;; Variables


(defvar claude-code-emacs-mcp-connection nil
  "Network connection to MCP server.")

(defvar claude-code-emacs-mcp-request-id 0
  "Counter for JSON-RPC request IDs.")

(defvar claude-code-emacs-mcp-pending-requests (make-hash-table :test 'equal)
  "Hash table of pending requests.")


;;; Connection Management

;;; Network Connection

(defun claude-code-emacs-mcp-connect ()
  "Connect to MCP server WebSocket."
  (condition-case err
      (progn
        (setq claude-code-emacs-mcp-connection
              (open-network-stream "claude-code-emacs-mcp"
                                   nil
                                   claude-code-emacs-mcp-host
                                   claude-code-emacs-mcp-port))
        (when claude-code-emacs-mcp-connection
          (set-process-filter claude-code-emacs-mcp-connection
                              #'claude-code-emacs-mcp-connection-filter)
          (set-process-sentinel claude-code-emacs-mcp-connection
                                #'claude-code-emacs-mcp-connection-sentinel)
          (message "Connected to MCP server WebSocket on port %d" claude-code-emacs-mcp-port)
          t))
    (error
     (message "Failed to connect to MCP server WebSocket: %s" err)
     nil)))

(defun claude-code-emacs-mcp-disconnect ()
  "Disconnect from MCP server."
  (interactive)
  (when claude-code-emacs-mcp-connection
    (delete-process claude-code-emacs-mcp-connection)
    (setq claude-code-emacs-mcp-connection nil))
  (clrhash claude-code-emacs-mcp-pending-requests)
  (message "Disconnected from MCP server"))

(defun claude-code-emacs-mcp-connection-sentinel (process event)
  "Handle connection events."
  (unless (process-live-p process)
    (setq claude-code-emacs-mcp-connection nil)
    (message "MCP WebSocket connection closed: %s" (string-trim event))))

(defvar claude-code-emacs-mcp-response-buffer ""
  "Buffer for accumulating partial responses.")

(defun claude-code-emacs-mcp-connection-filter (_process output)
  "Handle data from MCP server."
  (setq claude-code-emacs-mcp-response-buffer
        (concat claude-code-emacs-mcp-response-buffer output))

  ;; Process complete messages (delimited by newlines)
  (let ((messages (split-string claude-code-emacs-mcp-response-buffer "\n")))
    (setq claude-code-emacs-mcp-response-buffer (car (last messages)))
    (dolist (msg (butlast messages))
      (when (> (length msg) 0)
        (claude-code-emacs-mcp-handle-message msg)))))

;;; JSON-RPC Communication

(defun claude-code-emacs-mcp-next-request-id ()
  "Get next request ID."
  (setq claude-code-emacs-mcp-request-id (1+ claude-code-emacs-mcp-request-id)))

(defun claude-code-emacs-mcp-send-request (method params callback)
  "Send JSON-RPC request with METHOD and PARAMS, call CALLBACK with result."
  (unless (and claude-code-emacs-mcp-connection
               (process-live-p claude-code-emacs-mcp-connection))
    (claude-code-emacs-mcp-ensure-connection)
    (unless claude-code-emacs-mcp-connection
      (error "Cannot connect to MCP server")))

  (let* ((id (claude-code-emacs-mcp-next-request-id))
         (request (json-encode
                   `((jsonrpc . "2.0")
                     (id . ,id)
                     (method . ,method)
                     (params . ,params)))))

    (puthash id callback claude-code-emacs-mcp-pending-requests)
    (process-send-string claude-code-emacs-mcp-connection
                         (concat request "\n"))))

(defun claude-code-emacs-mcp-send-notification (method params)
  "Send JSON-RPC notification with METHOD and PARAMS."
  (unless (and claude-code-emacs-mcp-connection
               (process-live-p claude-code-emacs-mcp-connection))
    (claude-code-emacs-mcp-ensure-connection)
    (unless claude-code-emacs-mcp-connection
      (error "Cannot connect to MCP server")))

  (let ((notification (json-encode
                       `((jsonrpc . "2.0")
                         (method . ,method)
                         (params . ,params)))))
    (process-send-string claude-code-emacs-mcp-connection
                         (concat notification "\n"))))

(defun claude-code-emacs-mcp-handle-message (message)
  "Handle incoming JSON-RPC message."
  (condition-case err
      (let* ((json-object-type 'alist)
             (json-array-type 'list)
             (msg (json-read-from-string message)))
        (cond
         ;; Response to our request
         ((assoc 'id msg)
          (let* ((id (cdr (assoc 'id msg)))
                 (callback (gethash id claude-code-emacs-mcp-pending-requests)))
            (when callback
              (remhash id claude-code-emacs-mcp-pending-requests)
              (if (assoc 'error msg)
                  (funcall callback nil (cdr (assoc 'error msg)))
                (funcall callback (cdr (assoc 'result msg)) nil)))))

         ;; Request from server
         ((assoc 'method msg)
          (claude-code-emacs-mcp-handle-request msg))

         ;; Invalid message
         (t
          (message "Invalid JSON-RPC message: %s" message))))
    (error
     (message "Error handling MCP message: %s" err))))

(defun claude-code-emacs-mcp-handle-request (request)
  "Handle incoming request from MCP server."
  (let* ((id (cdr (assoc 'id request)))
         (method (cdr (assoc 'method request)))
         (params (cdr (assoc 'params request)))
         (handler (intern (format "claude-code-emacs-mcp-handle-%s" method))))

    (if (fboundp handler)
        (condition-case err
            (let ((result (funcall handler params)))
              (claude-code-emacs-mcp-send-response id result nil))
          (error
           (claude-code-emacs-mcp-send-response id nil
                                                `((code . -32603)
                                                  (message . ,(error-message-string err))))))
      (claude-code-emacs-mcp-send-response id nil
                                           `((code . -32601)
                                             (message . ,(format "Method not found: %s" method)))))))

(defun claude-code-emacs-mcp-send-response (id result error)
  "Send response for request ID with RESULT or ERROR."
  (let ((response (if error
                      `((jsonrpc . "2.0")
                        (id . ,id)
                        (error . ,error))
                    `((jsonrpc . "2.0")
                      (id . ,id)
                      (result . ,result)))))
    (process-send-string claude-code-emacs-mcp-connection
                         (concat (json-encode response) "\n"))))

;;; MCP Tool Handlers

(defun claude-code-emacs-mcp-handle-openFile (params)
  "Handle openFile request with PARAMS."
  (let* ((path (cdr (assoc 'path params)))
         (start-text (cdr (assoc 'startText params)))
         (end-text (cdr (assoc 'endText params)))
         (full-path (expand-file-name path (projectile-project-root))))

    (unless (file-exists-p full-path)
      (signal 'file-missing (list "File not found" full-path)))

    (find-file full-path)

    ;; Handle text selection if specified
    (when (and start-text end-text)
      (goto-char (point-min))
      (when (search-forward start-text nil t)
        (let ((start (match-beginning 0)))
          (when (search-forward end-text nil t)
            (let ((end (point)))
              (goto-char start)
              (set-mark end))))))

    `((success . t)
      (path . ,full-path))))

(defun claude-code-emacs-mcp-handle-getOpenBuffers (params)
  "Handle getOpenBuffers request."
  (let* ((include-hidden (cdr (assoc 'includeHidden params)))
         (project-root (projectile-project-root))
         (buffers '()))

    (dolist (buffer (buffer-list))
      (let ((file-path (buffer-file-name buffer))
            (buffer-name (buffer-name buffer)))
        (when (and file-path
                   (string-prefix-p project-root file-path)
                   (or include-hidden
                       (not (string-prefix-p " " buffer-name))))
          (push `((path . ,file-path)
                  (name . ,buffer-name)
                  (active . ,(eq buffer (current-buffer)))
                  (modified . ,(buffer-modified-p buffer)))
                buffers))))

    `((buffers . ,(nreverse buffers)))))

(defun claude-code-emacs-mcp-handle-getCurrentSelection (_params)
  "Handle getCurrentSelection request."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end))
             (start-line (line-number-at-pos start))
             (end-line (line-number-at-pos end))
             (start-char (save-excursion
                           (goto-char start)
                           (current-column)))
             (end-char (save-excursion
                         (goto-char end)
                         (current-column))))
        `((text . ,text)
          (startLine . ,start-line)
          (endLine . ,end-line)
          (startChar . ,start-char)
          (endChar . ,end-char)
          (fileName . ,(or (buffer-file-name) ""))))
    `((text . "")
      (startLine . 0)
      (endLine . 0)
      (startChar . 0)
      (endChar . 0)
      (fileName . ""))))

(defun claude-code-emacs-mcp-handle-getDiagnostics (params)
  "Handle getDiagnostics request."
  (let* ((buffer-path (cdr (assoc 'bufferPath params)))
         (diagnostics '()))

    (when (and (fboundp 'lsp-diagnostics)
               (fboundp 'lsp:diagnostic-message))
      (let ((lsp-diags (if buffer-path
                           (with-current-buffer (find-buffer-visiting buffer-path)
                             (lsp-diagnostics))
                         (lsp-diagnostics))))
        (maphash
         (lambda (file diags-by-line)
           (maphash
            (lambda (line diags)
              (dolist (diag diags)
                (push `((file . ,file)
                        (line . ,line)
                        (column . ,(if (and (fboundp 'lsp:position-character)
                                            (fboundp 'lsp:range-start)
                                            (fboundp 'lsp:diagnostic-range))
                                       (or (lsp:position-character
                                            (lsp:range-start
                                             (lsp:diagnostic-range diag)))
                                           0)
                                     0))
                        (severity . ,(if (fboundp 'lsp:diagnostic-severity)
                                         (pcase (lsp:diagnostic-severity diag)
                                           (1 "error")
                                           (2 "warning")
                                           (_ "info"))
                                       "info"))
                        (message . ,(lsp:diagnostic-message diag))
                        (source . ,(if (fboundp 'lsp:diagnostic-source)
                                       (or (lsp:diagnostic-source diag) "lsp")
                                     "lsp")))
                      diagnostics)))
            diags-by-line))
         lsp-diags)))

    `((diagnostics . ,(nreverse diagnostics)))))

;;; Integration with main Claude Code Emacs

(defun claude-code-emacs-mcp-ensure-connection ()
  "Ensure connection to MCP server."
  (unless (and claude-code-emacs-mcp-connection
               (process-live-p claude-code-emacs-mcp-connection))
    (claude-code-emacs-mcp-connect)))

;; Hook into Claude Code session start to ensure connection
(add-hook 'claude-code-emacs-vterm-mode-hook
          #'claude-code-emacs-mcp-ensure-connection)

(provide 'claude-code-emacs-mcp)
;;; claude-code-emacs-mcp.el ends here
