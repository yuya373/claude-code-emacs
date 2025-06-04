;;; claude-code-emacs-buffer.el --- Buffer management for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yuya Minami
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "0.0.2"))

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

(defvar claude-code-emacs-chunk-delay)  ;; Defined in core
(defvar claude-code-emacs-executable)   ;; Defined in core

;;; Buffer Operations

(defun claude-code-emacs-send-string (string &optional paste-p)
  "Send STRING to the Claude Code session."
  (interactive "sEnter text: ")
  (let ((chunks (claude-code-emacs-chunk-string string)))
    (claude-code-emacs-with-vterm-buffer
     (lambda ()
       (if (= (length chunks) 1)
           ;; Single chunk, send directly
           (vterm-send-string (car chunks) paste-p)
         ;; Multiple chunks, send with delay
         (dolist (chunk chunks)
           (vterm-send-string chunk paste-p)
           (sit-for claude-code-emacs-chunk-delay)))
       (vterm-send-return)))))

(defun claude-code-emacs-send-return ()
  "Send a return key to Claude Code session."
  (interactive)
  (claude-code-emacs-with-vterm-buffer
   #'vterm-send-return))

(defun claude-code-emacs-send-escape ()
  "Send an escape key to Claude Code session."
  (interactive)
  (claude-code-emacs-with-vterm-buffer
   #'vterm-send-escape))

(defun claude-code-emacs-send-ctrl-r ()
  "Send Ctrl-R to Claude Code session for retry."
  (interactive)
  (claude-code-emacs-with-vterm-buffer
    (vterm-send-key "r" nil nil t)))

(defun claude-code-emacs-send-number (n)
  "Send number N to Claude Code session."
  (interactive "nEnter number: ")
  (claude-code-emacs-send-string (number-to-string n)))

(defun claude-code-emacs-send-yes ()
  "Send \\='y\\=' to Claude Code session."
  (interactive)
  (claude-code-emacs-send-string "y"))

(defun claude-code-emacs-send-1 ()
  "Send '1' to Claude Code session."
  (interactive)
  (claude-code-emacs-send-string "1"))

(defun claude-code-emacs-send-2 ()
  "Send '2' to Claude Code session."
  (interactive)
  (claude-code-emacs-send-string "2"))

(defun claude-code-emacs-send-3 ()
  "Send '3' to Claude Code session."
  (interactive)
  (claude-code-emacs-send-string "3"))

(defun claude-code-emacs-send-region ()
  "Send the selected region to Claude Code session."
  (interactive)
  (if (use-region-p)
      (claude-code-emacs-send-string (buffer-substring-no-properties (region-beginning) (region-end)))
    (error "No region selected")))

;;; File Path Utilities

(defun claude-code-emacs-get-buffer-paths ()
  "Get all file paths from buffers in the current project."
  (let ((project-root (projectile-project-root))
        (paths '()))
    (when project-root
      (dolist (buffer (buffer-list))
        (when-let* ((file-path (buffer-file-name buffer)))
          (when (string-prefix-p project-root file-path)
            (push file-path paths)))))
    (sort paths #'string<)))

(defun claude-code-emacs-format-buffer-path (path)
  "Format PATH relative to project root with @ prefix."
  (let ((project-root (projectile-project-root)))
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
