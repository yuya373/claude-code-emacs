;;; claude-code-edit.el --- Special edit buffer for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: George Mauer
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

;; This module provides a special edit buffer for composing messages to Claude Code.
;; Similar to `org-edit-special', it opens a dedicated buffer where you can edit
;; text with full Emacs capabilities before sending it to the Claude Code vterm.
;;
;; Usage:
;;   M-x claude-code-edit-special  ; Open edit buffer
;;   C-c C-c                       ; Send and close
;;   C-c C-k                       ; Cancel without sending

;;; Code:

(require 'markdown-mode)

;; Forward declarations
(declare-function claude-code-send-string "claude-code-core" (string &optional paste-p))
(declare-function claude-code-normalize-project-root "claude-code-core" (project-root))
(declare-function claude-code-ensure-buffer "claude-code-core" ())
(declare-function projectile-project-root "projectile" ())

;;; Customization

(defgroup claude-code-edit nil
  "Special edit buffer for Claude Code."
  :group 'claude-code
  :prefix "claude-code-edit-")

(defcustom claude-code-edit-buffer-window-setup 'reorganize-frame
  "How to display the edit buffer.
Possible values are:
  current-window         Show edit buffer in current window
  other-window           Show edit buffer in another window
  reorganize-frame       Show edit buffer in another window and maximize
  other-frame            Show edit buffer in another frame

The default is `reorganize-frame' which provides a focused editing experience."
  :type '(choice
          (const :tag "current-window" current-window)
          (const :tag "other-window" other-window)
          (const :tag "reorganize-frame" reorganize-frame)
          (const :tag "other-frame" other-frame))
  :group 'claude-code-edit)

;;; Buffer management

(defun claude-code-edit-buffer-name ()
  "Return the buffer name for Claude Code edit buffer in current project.
Return nil if not in a project."
  (when-let ((project-root (claude-code-normalize-project-root (projectile-project-root))))
    (format "*claude-code-edit:%s*" project-root)))

(defun claude-code-edit-get-buffer ()
  "Get the Claude Code edit buffer for current project.
Return nil if it doesn't exist."
  (get-buffer (claude-code-edit-buffer-name)))

(defun claude-code-edit-get-or-create-buffer ()
  "Get or create the Claude Code edit buffer for the current project."
  (let ((buf-name (claude-code-edit-buffer-name)))
    (or (get-buffer buf-name)
        (get-buffer-create buf-name))))

;;; Edit buffer display

(defun claude-code-edit-display-buffer (buffer)
  "Display edit BUFFER according to `claude-code-edit-buffer-window-setup'."
  (pcase claude-code-edit-buffer-window-setup
    ('current-window
     (switch-to-buffer buffer))
    ('other-window
     (switch-to-buffer-other-window buffer))
    ('reorganize-frame
     (delete-other-windows)
     (switch-to-buffer-other-window buffer))
    ('other-frame
     (switch-to-buffer-other-frame buffer))
    (_
     (switch-to-buffer-other-window buffer))))

;;; Mode definition

(defvar claude-code-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'claude-code-edit-finish)
    (define-key map (kbd "C-c C-k") 'claude-code-edit-cancel)
    map)
  "Keymap for `claude-code-edit-mode'.")

(define-minor-mode claude-code-edit-mode
  "Minor mode for Claude Code edit buffers.

\\{claude-code-edit-mode-map}"
  :lighter " Claude-Edit"
  :keymap claude-code-edit-mode-map
  (when claude-code-edit-mode
    (setq-local header-line-format
                '(:eval (format "Claude Code Edit - %s | C-c C-c to send, C-c C-k to cancel"
                                (file-name-nondirectory
                                 (directory-file-name
                                  (claude-code-normalize-project-root (projectile-project-root)))))))))

;;; Main commands

;;;###autoload
(defun claude-code-edit-special ()
  "Open a special edit buffer for composing Claude Code messages.
Similar to `org-edit-special', this provides a dedicated buffer
for editing text before sending it to Claude Code.

Use \\[claude-code-edit-finish] (C-c C-c) to send the buffer contents
to Claude Code and close the edit buffer.

Use \\[claude-code-edit-cancel] (C-c C-k) to close the edit buffer
without sending anything."
  (interactive)
  ;; Ensure there's a Claude Code session running
  ;; Try to find any claude-code buffer if the standard naming fails
  (condition-case nil
      (claude-code-ensure-buffer)
    (error
     (unless (cl-some (lambda (buf)
                        (string-match-p "\\*claude" (buffer-name buf)))
                      (buffer-list))
       (error "No Claude Code session found.  Use 'claude-code-run' to start one"))))

  (let* ((edit-buffer (claude-code-edit-get-or-create-buffer))
         (existing-content (with-current-buffer edit-buffer
                             (buffer-string))))
    ;; Display the buffer
    (claude-code-edit-display-buffer edit-buffer)

    ;; Set up the buffer if it's new
    (with-current-buffer edit-buffer
      (unless (derived-mode-p 'markdown-mode)
        (markdown-mode))
      (claude-code-edit-mode 1)

      ;; If buffer is empty, add helpful hint
      (when (string-empty-p (string-trim existing-content))
        (insert "<!-- Compose your message to Claude Code here -->\n\n")
        (goto-char (point-max))))))

;;;###autoload
(defun claude-code-edit-finish ()
  "Send the edit buffer contents to Claude Code and close the buffer."
  (interactive)
  (unless claude-code-edit-mode
    (user-error "Not in a Claude Code edit buffer"))

  (let* ((content (buffer-string))
         (trimmed (string-trim content)))
    (if (string-empty-p trimmed)
        (message "Edit buffer is empty, nothing to send")
      ;; Find any Claude Code buffer and send to it
      (let ((claude-buffer (or (claude-code-get-buffer)
                               ;; Fall back to finding any claude-code vterm buffer (but NOT edit buffers)
                               (cl-find-if (lambda (buf)
                                             (and (string-match-p "\\*claude-?code\\[" (buffer-name buf))
                                                  (not (string-match-p "edit" (buffer-name buf)))))
                                           (buffer-list)))))
        (if (not claude-buffer)
            (message "Failed to send to Claude Code: No Claude Code session found")
          ;; Send directly to the vterm buffer using the same approach as claude-code-send-string
          (condition-case err
              (progn
                (with-current-buffer claude-buffer
                  (require 'vterm)
                  (goto-char (point-max))  ; Make sure we're at the end
                  (vterm-send-string trimmed t)  ; Use paste mode
                  ;; Wait for processing
                  (when (boundp 'vterm-timer-delay)
                    (sit-for (* vterm-timer-delay 3)))
                  (vterm-send-return))
                (message "Sent %d characters to Claude Code" (length trimmed))
                ;; Clear the buffer for next use
                (erase-buffer)
                ;; Close the edit buffer window
                (when-let ((window (get-buffer-window (current-buffer))))
                  (quit-window nil window)))
            (error
             (message "Failed to send to Claude Code: %s" (error-message-string err)))))))))

;;;###autoload
(defun claude-code-edit-cancel ()
  "Close the edit buffer without sending anything.
The buffer contents are preserved for the next edit session."
  (interactive)
  (unless claude-code-edit-mode
    (user-error "Not in a Claude Code edit buffer"))

  (when-let ((window (get-buffer-window (current-buffer))))
    (quit-window nil window))
  (message "Edit cancelled"))

(provide 'claude-code-edit)
;;; claude-code-edit.el ends here
