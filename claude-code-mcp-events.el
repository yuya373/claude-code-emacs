;;; claude-code-mcp-events.el --- MCP event handlers for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude <noreply@anthropic.com>
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

;; This module implements event handlers that notify Claude Code CLI about
;; Emacs state changes via MCP:
;; - buffer-list-update-hook: Track buffer list changes
;; - after-change-functions: Track buffer content modifications
;; - lsp-diagnostics-updated-hook: Track diagnostics changes

;;; Code:

(require 'claude-code-core)
(require 'claude-code-mcp-connection)
(require 'projectile)
(require 'lsp-mode nil t)
(require 'lsp-protocol nil t)

;; LSP function declarations
(declare-function lsp:diagnostic-range "lsp-protocol" (diagnostic))
(declare-function lsp:range-start "lsp-protocol" (range))
(declare-function lsp:position-line "lsp-protocol" (position))
(declare-function lsp:position-character "lsp-protocol" (position))
(declare-function lsp:diagnostic-severity? "lsp-protocol" (diagnostic))
(declare-function lsp:diagnostic-message "lsp-protocol" (diagnostic))

(defvar claude-code-mcp-events-buffer-change-timer nil
  "Timer for debouncing buffer change notifications.")

(defvar claude-code-mcp-events-buffer-list-timer nil
  "Timer for debouncing buffer list update notifications.")

(defvar claude-code-mcp-events-diagnostics-timer nil
  "Timer for debouncing diagnostics update notifications.")

(defvar claude-code-mcp-events-pending-changes nil
  "Alist of pending buffer changes waiting to be sent.")

(defvar claude-code-mcp-events-enabled t
  "Whether event notifications are enabled.")

(defcustom claude-code-mcp-events-change-delay 0.5
  "Delay in seconds before sending buffer change notifications."
  :type 'number
  :group 'claude-code)

(defcustom claude-code-mcp-events-buffer-list-delay 1.0
  "Delay in seconds before sending buffer list update notifications."
  :type 'number
  :group 'claude-code)

(defcustom claude-code-mcp-events-diagnostics-delay 1.0
  "Delay in seconds before sending diagnostics update notifications."
  :type 'number
  :group 'claude-code)

;;; Buffer List Update Handler

(defun claude-code-mcp-events-buffer-list-updated ()
  "Handle buffer list update events."
  (when claude-code-mcp-events-enabled
    ;; Cancel existing timer
    (when claude-code-mcp-events-buffer-list-timer
      (cancel-timer claude-code-mcp-events-buffer-list-timer))

    ;; Set new timer to debounce rapid changes
    (setq claude-code-mcp-events-buffer-list-timer
          (run-with-timer claude-code-mcp-events-buffer-list-delay nil
                          #'claude-code-mcp-events-send-buffer-list-update))))

(defun claude-code-mcp-events-send-buffer-list-update ()
  "Send buffer list update notification to all connected MCP servers."
  (condition-case err
      ;; Iterate through all project connections
      (maphash
       (lambda (project-root _info)
         ;; Collect buffers for this project
         (let ((buffers '()))
           (dolist (buffer (buffer-list))
             (let ((file-path (buffer-file-name buffer))
                   (buffer-name (buffer-name buffer)))
               (when (and file-path
                          (string-prefix-p project-root file-path)
                          (not (string-prefix-p " " buffer-name)))
                 (push `((path . ,file-path)
                         (name . ,buffer-name)
                         (active . ,(eq buffer (current-buffer)))
                         (modified . ,(buffer-modified-p buffer)))
                       buffers))))

           ;; Send notification for this project if there are buffers
           (when buffers
             (claude-code-mcp-send-event-to-project
              project-root
              "bufferListUpdated"
              `((buffers . ,(nreverse buffers)))))))
       claude-code-mcp-project-connections)
    (error
     (message "Error sending buffer list update: %s" (error-message-string err)))))

;;; Buffer Content Change Handler

(defun claude-code-mcp-events-after-change (beg end old-len)
  "Handle buffer content change.
BEG and END are the beginning and end of the changed region.
OLD-LEN is the length of the text before the change."
  (when (and claude-code-mcp-events-enabled
             (buffer-file-name))
    (let ((project-root (ignore-errors (claude-code-normalize-project-root (projectile-project-root)))))
      (when (and project-root
                 (string-prefix-p project-root (buffer-file-name)))
        ;; Store change information
        (let* ((file (buffer-file-name))
               (start-line (line-number-at-pos beg))
               (end-line (line-number-at-pos end))
               (existing (assoc file claude-code-mcp-events-pending-changes)))

          (if existing
              ;; Update existing change info with expanded range
              (let ((old-start (nth 1 existing))
                    (old-end (nth 2 existing)))
                (setcdr existing
                        (list (min start-line old-start)
                              (max end-line old-end)
                              old-len
                              project-root)))
            ;; Add new change info with project root
            (push (list file start-line end-line old-len project-root)
                  claude-code-mcp-events-pending-changes)))

        ;; Cancel existing timer
        (when claude-code-mcp-events-buffer-change-timer
          (cancel-timer claude-code-mcp-events-buffer-change-timer))

        ;; Set new timer
        (setq claude-code-mcp-events-buffer-change-timer
              (run-with-timer claude-code-mcp-events-change-delay nil
                              #'claude-code-mcp-events-send-buffer-changes))))))

(defun claude-code-mcp-events-send-buffer-changes ()
  "Send pending buffer change notifications."
  (condition-case err
      (when claude-code-mcp-events-pending-changes
        ;; Group changes by project
        (let ((changes-by-project (make-hash-table :test 'equal)))
          ;; Group pending changes by project root (project root is now stored in the change info)
          (dolist (change claude-code-mcp-events-pending-changes)
            (let* ((project-root (nth 4 change)))  ;; Project root is now the 5th element
              (when project-root
                (push change (gethash project-root changes-by-project)))))

          ;; Send changes for each project (all changes as a batch per project)
          (maphash
           (lambda (project-root changes)
             ;; Convert changes to the format expected by the notification
             (let ((formatted-changes
                    (mapcar (lambda (change)
                              (let ((file (nth 0 change))
                                    (start-line (nth 1 change))
                                    (end-line (nth 2 change))
                                    (old-len (nth 3 change)))
                                `((file . ,file)
                                  (startLine . ,start-line)
                                  (endLine . ,end-line)
                                  (changeLength . ,old-len))))
                            changes)))
               ;; Send all changes for this project at once
               (claude-code-mcp-send-event-to-project
                project-root
                "bufferContentModified"
                `((changes . ,formatted-changes)))))
           changes-by-project))
        ;; Clear pending changes
        (setq claude-code-mcp-events-pending-changes nil))
    (error
     (message "Error sending buffer changes: %s" (error-message-string err)))))

;;; Diagnostics Update Handler

(defun claude-code-mcp-events-diagnostics-updated ()
  "Handle LSP diagnostics update events."
  (when claude-code-mcp-events-enabled
    ;; Cancel existing timer
    (when claude-code-mcp-events-diagnostics-timer
      (cancel-timer claude-code-mcp-events-diagnostics-timer))

    ;; Set new timer
    (setq claude-code-mcp-events-diagnostics-timer
          (run-with-timer claude-code-mcp-events-diagnostics-delay nil
                          #'claude-code-mcp-events-send-diagnostics-update))))

(defun claude-code-mcp-events-send-diagnostics-update ()
  "Send diagnostics update notification to all relevant MCP servers."
  (condition-case err
      (when (fboundp 'lsp-diagnostics)
        ;; Group diagnostics by project
        (let ((diagnostics-by-project (make-hash-table :test 'equal))
              (lsp-diags (ignore-errors (lsp-diagnostics))))
          (when lsp-diags
            ;; First, group all diagnostics by their project root
            (maphash
             (lambda (file diags)
               (let ((project-root (ignore-errors
                                     (with-current-buffer (or (find-buffer-visiting file)
                                                              (find-file-noselect file))
                                       (claude-code-normalize-project-root
                                        (projectile-project-root))))))
                 (when project-root
                   (let ((file-diagnostics '()))
                     (dolist (diag diags)
                       (let* ((range (lsp:diagnostic-range diag))
                              (start (lsp:range-start range))
                              (line (1+ (lsp:position-line start))))
                         (push `((line . ,line)
                                 (column . ,(lsp:position-character start))
                                 (severity . ,(pcase (lsp:diagnostic-severity? diag)
                                                (1 "error")
                                                (2 "warning")
                                                (3 "information")
                                                (4 "hint")
                                                (5 "max")
                                                (_ "")))
                                 (message . ,(lsp:diagnostic-message diag)))
                               file-diagnostics)))
                     (when file-diagnostics
                       ;; Store diagnostics by project and file
                       (let ((project-files (or (gethash project-root diagnostics-by-project)
                                                (make-hash-table :test 'equal))))
                         (puthash file (nreverse file-diagnostics) project-files)
                         (puthash project-root project-files diagnostics-by-project)))))))
             lsp-diags)

            ;; Send notifications for each project (all diagnostics as a batch per project)
            (maphash
             (lambda (project-root project-files)
               ;; Convert hash table to list of file diagnostics
               (let ((all-diagnostics '()))
                 (maphash
                  (lambda (file diagnostics)
                    (push `((file . ,file)
                            (diagnostics . ,diagnostics))
                          all-diagnostics))
                  project-files)
                 ;; Send all diagnostics for this project at once
                 (when all-diagnostics
                   (claude-code-mcp-send-event-to-project
                    project-root
                    "diagnosticsChanged"
                    `((files . ,(nreverse all-diagnostics)))))))
             diagnostics-by-project))))
    (error
     (message "Error sending diagnostics update: %s" (error-message-string err)))))

;;; Hook Management

;;;###autoload
(defun claude-code-mcp-events-enable ()
  "Enable MCP event notifications."
  (interactive)
  (setq claude-code-mcp-events-enabled t)
  ;; Add hooks
  (add-hook 'buffer-list-update-hook #'claude-code-mcp-events-buffer-list-updated)
  (add-hook 'after-change-functions #'claude-code-mcp-events-after-change)
  (when (fboundp 'lsp-diagnostics-updated-hook)
    (add-hook 'lsp-diagnostics-updated-hook #'claude-code-mcp-events-diagnostics-updated))
  (message "Claude Code MCP event notifications enabled"))

;;;###autoload
(defun claude-code-mcp-events-disable ()
  "Disable MCP event notifications."
  (interactive)
  (setq claude-code-mcp-events-enabled nil)
  ;; Remove hooks
  (remove-hook 'buffer-list-update-hook #'claude-code-mcp-events-buffer-list-updated)
  (remove-hook 'after-change-functions #'claude-code-mcp-events-after-change)
  (when (fboundp 'lsp-diagnostics-updated-hook)
    (remove-hook 'lsp-diagnostics-updated-hook #'claude-code-mcp-events-diagnostics-updated))
  ;; Cancel any pending timers
  (when claude-code-mcp-events-buffer-change-timer
    (cancel-timer claude-code-mcp-events-buffer-change-timer))
  (when claude-code-mcp-events-buffer-list-timer
    (cancel-timer claude-code-mcp-events-buffer-list-timer))
  (when claude-code-mcp-events-diagnostics-timer
    (cancel-timer claude-code-mcp-events-diagnostics-timer))
  (message "Claude Code MCP event notifications disabled"))

;;; Initialize

;; Don't enable by default - let users decide in their config
;; Users can add (claude-code-mcp-events-enable) to their init file

(provide 'claude-code-mcp-events)
;;; claude-code-mcp-events.el ends here
