;;; claude-code-emacs-session.el --- Session management for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yuya Minami
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.2") (projectile "2.5.0"))

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

;; Session management functionality for Claude Code Emacs.

;;; Code:

(require 'vterm)
(require 'projectile)
(require 'claude-code-emacs-core)

;; Forward declarations
(declare-function claude-code-emacs-vterm-mode "claude-code-emacs-ui" ())
(declare-function claude-code-emacs-mcp-maybe-ensure-connection "claude-code-emacs-mcp" ())
(declare-function claude-code-emacs-mcp-disconnect "claude-code-emacs-mcp-connection" (&optional project-root))
(declare-function claude-code-emacs-mcp-unregister-port "claude-code-emacs-mcp-connection" (project-root))

;;; Session Management

;;;###autoload
(defun claude-code-emacs ()
  "Start or switch to Claude Code session for current project."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (buffer-name (claude-code-emacs-buffer-name))
         (existing-buffer (get-buffer buffer-name)))
    (if existing-buffer
        ;; Buffer exists, just switch to it
        (switch-to-buffer existing-buffer)
      ;; Create new session
      (let ((default-directory (or project-root default-directory)))
        (vterm buffer-name)
        (with-current-buffer buffer-name
          (claude-code-emacs-vterm-mode)
          ;; Start Claude Code in the vterm
          (vterm-send-string (concat claude-code-emacs-executable " code"))
          (vterm-send-return)
          ;; Try to establish MCP connection if available
          (when (fboundp 'claude-code-emacs-mcp-maybe-ensure-connection)
            (claude-code-emacs-mcp-maybe-ensure-connection)))))))

(defun claude-code-emacs-switch-to-buffer ()
  "Switch to Claude Code buffer for current project."
  (interactive)
  (if-let* ((buffer (claude-code-emacs-get-buffer)))
      (switch-to-buffer buffer)
    (if (y-or-n-p "No Claude Code session for current project. Start one? ")
        (claude-code-emacs)
      (message "Cancelled"))))

(defun claude-code-emacs-close ()
  "Close the Claude Code window for current project."
  (interactive)
  (when-let* ((buffer (claude-code-emacs-get-buffer)))
    (let ((windows (get-buffer-window-list buffer)))
      (dolist (window windows)
        (delete-window window)))))

(defun claude-code-emacs-quit ()
  "Send /quit command to Claude Code and kill the session."
  (interactive)
  (when-let* ((buffer (claude-code-emacs-get-buffer)))
    (with-current-buffer buffer
      ;; Send /quit command
      (vterm-send-string "/quit")
      (vterm-send-return)
      ;; Give the process a moment to exit cleanly
      (run-at-time 0.5 nil
                   (lambda ()
                     (when (buffer-live-p buffer)
                       ;; Kill vterm process if still running
                       (when (and (boundp 'vterm--process)
                                  vterm--process
                                  (process-live-p vterm--process))
                         (kill-process vterm--process))
                       ;; Clean up MCP connection if it exists
                       (when (and (fboundp 'claude-code-emacs-mcp-disconnect)
                                  (fboundp 'claude-code-emacs-mcp-unregister-port))
                         (let ((project-root (projectile-project-root)))
                           (claude-code-emacs-mcp-disconnect project-root)
                           (claude-code-emacs-mcp-unregister-port project-root)))
                       ;; Kill the buffer
                       (kill-buffer buffer)))))))

(defun claude-code-emacs-restart ()
  "Restart Claude Code session for current project."
  (interactive)
  (claude-code-emacs-quit)
  (sit-for 1.0)  ; Brief pause before restarting
  (claude-code-emacs))

;;; Window Management

(defun claude-code-emacs-other-window ()
  "Start or switch to Claude Code session in other window."
  (interactive)
  (let* ((buffer-name (claude-code-emacs-buffer-name))
         (existing-buffer (get-buffer buffer-name)))
    (if existing-buffer
        (switch-to-buffer-other-window existing-buffer)
      ;; Start new session in other window
      (switch-to-buffer-other-window (current-buffer))
      (claude-code-emacs))))

(defun claude-code-emacs-split-window-below ()
  "Start or switch to Claude Code session in window below."
  (interactive)
  (split-window-below)
  (other-window 1)
  (claude-code-emacs-switch-to-buffer))

(defun claude-code-emacs-split-window-right ()
  "Start or switch to Claude Code session in window to the right."
  (interactive)
  (split-window-right)
  (other-window 1)
  (claude-code-emacs-switch-to-buffer))

(provide 'claude-code-emacs-session)
;;; claude-code-emacs-session.el ends here
