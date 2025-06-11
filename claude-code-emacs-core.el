;;; claude-code-emacs-core.el --- Core functionality for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "20241218.331") (projectile "2.9.1"))

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

;; Core functionality for Claude Code Emacs including:
;; - Buffer management functions
;; - String processing utilities
;; - Session lifecycle management

;;; Code:

(require 'vterm)
(require 'projectile)

;; Forward declarations for MCP integration
(declare-function claude-code-emacs-mcp-disconnect "claude-code-emacs-mcp-connection" (project-root))
(declare-function claude-code-emacs-vterm-mode "claude-code-emacs-ui" ())
(declare-function claude-code-emacs-send-string "claude-code-emacs-buffer" (string))

;;; Customization

(defgroup claude-code-emacs nil
  "Run Claude Code within Emacs."
  :group 'tools
  :prefix "claude-code-emacs-")

(defcustom claude-code-emacs-chunk-size 50
  "Size of chunks when sending long strings to vterm."
  :type 'integer
  :group 'claude-code-emacs)

(defcustom claude-code-emacs-chunk-delay 0.05
  "Delay in seconds between sending chunks to vterm."
  :type 'number
  :group 'claude-code-emacs)

(defcustom claude-code-emacs-executable "claude"
  "The executable name or path for Claude Code CLI."
  :type 'string
  :group 'claude-code-emacs)

(defconst claude-code-emacs-available-options
  '(("--verbose" . "Enable detailed logging")
    ("--model sonnet" . "Use Claude Sonnet model")
    ("--model opus" . "Use Claude Opus model")
    ("--resume" . "Resume specific session by ID")
    ("--continue" . "Load latest conversation in current directory")
    ("--dangerously-skip-permissions" . "Skip permission prompts"))
  "Available options for Claude Code CLI.")

;;; Buffer Management

(defun claude-code-emacs-normalize-project-root (project-root)
  "Normalize PROJECT-ROOT by removing trailing slash."
  (directory-file-name project-root))

(defun claude-code-emacs-buffer-name ()
  "Return the buffer name for Claude Code session in current project."
  (let ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root))))
    (format "*claude:%s*" project-root)))

(defun claude-code-emacs-get-buffer ()
  "Get the Claude Code buffer for the current project, or nil if it doesn't exist."
  (get-buffer (claude-code-emacs-buffer-name)))

(defun claude-code-emacs-ensure-buffer ()
  "Ensure Claude Code buffer exists, error if not."
  (or (claude-code-emacs-get-buffer)
      (error "No Claude Code session for this project. Use 'claude-code-emacs-run' to start one")))

(defun claude-code-emacs-with-vterm-buffer (body-fn)
  "Execute BODY-FN in the Claude Code vterm buffer."
  (let ((buf (claude-code-emacs-ensure-buffer)))
    (with-current-buffer buf
      (funcall body-fn))))

;;; String Processing

(defun claude-code-emacs-chunk-string (str &optional chunk-size)
  "Split STR into chunks of CHUNK-SIZE characters.
If CHUNK-SIZE is not provided, use `claude-code-emacs-chunk-size'."
  (let ((chunk-size (or chunk-size claude-code-emacs-chunk-size)))
    (if (or (<= chunk-size 0)
            (not (stringp str)))
        (error "Invalid input: string must be non-empty and chunk-size must be greater than 0")
      (if (string-empty-p str)
          '("")
        (let ((len (length str))
              (result '())
              (start 0))
          (while (< start len)
            (let ((end (min (+ start chunk-size) len)))
              (push (substring str start end) result)
              (setq start end)))
          (nreverse result))))))


;;; Session Management

;;;###autoload
(defun claude-code-emacs-run ()
  "Start Claude Code session for the current project.
With prefix argument, select from available options."
  (interactive)
  (let* ((buffer-name (claude-code-emacs-buffer-name))
         (project-root (claude-code-emacs-normalize-project-root (projectile-project-root)))
         (default-directory project-root)
         (buf (get-buffer-create buffer-name))
         (selected-option (when current-prefix-arg
                            (let* ((choices (mapcar (lambda (opt)
                                                      (format "%s - %s"
                                                              (car opt)
                                                              (cdr opt)))
                                                    claude-code-emacs-available-options))
                                   (selected (completing-read "Select Claude option: " choices nil t)))
                              (when selected
                                (car (split-string selected " - "))))))
         (extra-input (when (and selected-option
                                 (string-match-p "--resume" selected-option))
                        (read-string "Session ID: "))))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-code-emacs-vterm-mode)
        (claude-code-emacs-vterm-mode))
      ;; Wait for vterm to be ready
      (run-at-time 0.1 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (let ((command (concat claude-code-emacs-executable
                                                (when selected-option
                                                  (concat " " selected-option))
                                                (when extra-input
                                                  (concat " " extra-input)))))
                           (vterm-send-string command)
                           (vterm-send-return)))))))
    (switch-to-buffer-other-window buffer-name)))

;;;###autoload
(defun claude-code-emacs-switch-to-buffer ()
  "Switch to the Claude Code buffer for the current project."
  (interactive)
  (let ((buffer-name (claude-code-emacs-buffer-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (message "No Claude Code session for this project. Use 'claude-code-emacs-run' to start one."))))

;;;###autoload
(defun claude-code-emacs-close ()
  "Close the window displaying the Claude Code buffer for the current project."
  (interactive)
  (let* ((buffer-name (claude-code-emacs-buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (let ((window (get-buffer-window buffer)))
          (if window
              (delete-window window)
            (message "Claude Code buffer is not displayed in any window")))
      (message "No Claude Code buffer found for this project"))))

;;;###autoload
(defun claude-code-emacs-quit ()
  "Quit the Claude Code session for the current project and kill the buffer."
  (interactive)
  (let* ((buffer-name (claude-code-emacs-buffer-name))
         (buffer (get-buffer buffer-name)))
    (if buffer
        (progn
          ;; First close any windows showing the buffer
          (dolist (window (get-buffer-window-list buffer nil t))
            (delete-window window))
          ;; Kill the vterm process if it exists
          (with-current-buffer buffer
            (vterm-send-string "/quit")
            (vterm-send-return)
            (run-at-time 3 nil
                         (lambda ()
                           (when (buffer-live-p buffer)
                             ;; Kill vterm process if still running
                             (when (and (boundp 'vterm--process)
                                        vterm--process
                                        (process-live-p vterm--process))
                               (kill-process vterm--process))
                             ;; Kill the buffer
                             (let ((kill-buffer-query-functions nil))
                               (kill-buffer buffer)))
                           (message "Claude Code session ended for this project")))))
      (message "No Claude Code buffer found for this project"))))

;;;###autoload
(defun claude-code-emacs-send-region ()
  "Send selected region to Claude Code."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (claude-code-emacs-send-string text))
    (user-error "No region selected")))

(provide 'claude-code-emacs-core)
;;; claude-code-emacs-core.el ends here
