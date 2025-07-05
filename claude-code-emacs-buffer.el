;;; claude-code-emacs-buffer.el --- Buffer management for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yuya Minami
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

;; Buffer management and key sending functionality for Claude Code Emacs.

;;; Code:

(require 'vterm)
(require 'claude-code-emacs-core)

(defvar claude-code-emacs-executable)   ;; Defined in core

;;; Buffer Operations

(defun claude-code-emacs-send-string (string &optional paste-p)
  "Send STRING to the Claude Code session."
  (interactive "sEnter text: ")
  (claude-code-emacs-with-vterm-buffer
   (lambda ()
     (vterm-send-string string paste-p)
     ;; NOTE: wait for `accept-process-output' in `vterm-send-string'
     (sit-for (* vterm-timer-delay 3))
     (vterm-send-return))))

;;; File Path Utilities

(defun claude-code-emacs-get-buffer-paths ()
  "Get all file paths from buffers in the current project."
  (let ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root)))
        (paths '()))
    (when project-root
      (dolist (buffer (buffer-list))
        (when-let* ((file-path (buffer-file-name buffer)))
          (when (string-prefix-p project-root file-path)
            (push file-path paths)))))
    (sort paths #'string<)))

(defun claude-code-emacs-format-buffer-path (path)
  "Format PATH relative to project root with @ prefix."
  (let ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root))))
    (if (and project-root (string-prefix-p project-root path))
        (concat "@" (file-relative-name path project-root))
      path)))

(defun claude-code-emacs-insert-file-path (path)
  "Insert formatted file PATH at point."
  (let ((formatted (claude-code-emacs-format-buffer-path path)))
    (insert formatted)))

(defun claude-code-emacs-insert-open-buffer-paths ()
  "Insert all open buffer paths separated by spaces."
  (interactive)
  (let ((paths (claude-code-emacs-get-buffer-paths)))
    (if paths
        (let ((formatted-paths (mapconcat #'claude-code-emacs-format-buffer-path paths " ")))
          (claude-code-emacs-send-string formatted-paths))
      (message "No open files in current project"))))

(provide 'claude-code-emacs-buffer)
;;; claude-code-emacs-buffer.el ends here
