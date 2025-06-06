;;; claude-code-emacs-commands.el --- Slash commands and custom commands for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

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

;; This module provides slash command definitions and custom command execution
;; functionality for Claude Code Emacs. It includes:
;; - Built-in slash commands (/init, /clear, /help, etc.)
;; - Custom project commands from .claude/commands/*.md
;; - Global commands from ~/.claude/commands/*.md

;;; Code:

(require 'projectile)

;; Forward declarations
(declare-function claude-code-emacs-send-string "claude-code-emacs-core" (string &optional paste-p))
(declare-function claude-code-emacs-ensure-buffer "claude-code-emacs-core" ())
(declare-function claude-code-emacs-with-vterm-buffer "claude-code-emacs-core" (body-fn))
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-key "vterm" (key))

;;; Slash command definitions

(defmacro claude-code-emacs-define-slash-command (name command)
  "Define a function claude-code-emacs-NAME that sends COMMAND."
  `(defun ,(intern (format "claude-code-emacs-%s" name)) ()
     (interactive)
     (claude-code-emacs-send-string ,command)))

;; Helper for commands with optional arguments
(defun claude-code-emacs-send-command-with-optional-args (command prompt)
  "Send COMMAND with optional arguments prompted by PROMPT."
  (let ((args (read-string prompt)))
    (if (string-empty-p args)
        (claude-code-emacs-send-string command)
      (claude-code-emacs-send-string (format "%s %s" command args)))))

;; Define simple slash commands
(claude-code-emacs-define-slash-command "init" "/init")
(claude-code-emacs-define-slash-command "clear" "/clear")
(claude-code-emacs-define-slash-command "help" "/help")
(claude-code-emacs-define-slash-command "memory" "/memory")
(claude-code-emacs-define-slash-command "config" "/config")
(claude-code-emacs-define-slash-command "cost" "/cost")
(claude-code-emacs-define-slash-command "status" "/status")
(claude-code-emacs-define-slash-command "review" "/review")
(claude-code-emacs-define-slash-command "pr-comments" "/pr_comments")
(claude-code-emacs-define-slash-command "bug" "/bug")
(claude-code-emacs-define-slash-command "doctor" "/doctor")
(claude-code-emacs-define-slash-command "login" "/login")
(claude-code-emacs-define-slash-command "logout" "/logout")

(defun claude-code-emacs-compact (&optional instructions)
  "Send /compact command with optional INSTRUCTIONS."
  (interactive "sCompact instructions (optional): ")
  (if instructions
      (if (string-empty-p instructions)
          (claude-code-emacs-send-string "/compact")
        (claude-code-emacs-send-string (format "/compact %s" instructions)))
    (claude-code-emacs-send-command-with-optional-args
     "/compact" "Compact instructions (optional): ")))

;;; Key sending functions

;;;###autoload
(defun claude-code-emacs-send-escape ()
  "Send ESC key to Claude Code buffer."
  (interactive)
  (claude-code-emacs-with-vterm-buffer #'vterm-send-escape))

;;;###autoload
(defun claude-code-emacs-send-return ()
  "Send Return key to Claude Code buffer."
  (interactive)
  (claude-code-emacs-with-vterm-buffer #'vterm-send-return))

;;; Quick send functions

;;;###autoload
(defun claude-code-emacs-send-1 ()
  "Send '1' to Claude Code buffer."
  (interactive)
  (claude-code-emacs-send-string "1"))

;;;###autoload
(defun claude-code-emacs-send-2 ()
  "Send '2' to Claude Code buffer."
  (interactive)
  (claude-code-emacs-send-string "2"))

;;;###autoload
(defun claude-code-emacs-send-3 ()
  "Send '3' to Claude Code buffer."
  (interactive)
  (claude-code-emacs-send-string "3"))

;;;###autoload
(defun claude-code-emacs-send-commit ()
  "Send `commit' to Claude Code buffer."
  (interactive)
  (claude-code-emacs-send-string "commit"))

;;;###autoload
(defun claude-code-emacs-send-ctrl-r ()
  "Send Ctrl+R to Claude Code buffer to toggle expand."
  (interactive)
  (claude-code-emacs-with-vterm-buffer
   (lambda () (vterm-send-key (kbd "C-r")))))

;;;###autoload
(defun claude-code-emacs-send-shift-tab ()
  "Send Shift+Tab to Claude Code buffer to toggle auto accept."
  (interactive)
  (claude-code-emacs-with-vterm-buffer
   (lambda ()
     (vterm-send-key "<tab>" t))))

;;; Helper functions for command argument handling

(defun claude-code-emacs-count-arguments (template)
  "Count the number of $ARGUMENTS placeholders in TEMPLATE."
  (let ((count 0)
        (pos 0))
    (while (string-match "\\$ARGUMENTS" template pos)
      (setq count (1+ count)
            pos (match-end 0)))
    count))

(defun claude-code-emacs-prompt-for-arguments (command-name arg-count)
  "Prompt user for ARG-COUNT arguments for COMMAND-NAME.
Returns a list of arguments."
  (let ((args '()))
    (dotimes (i arg-count)
      (let ((prompt (if (= arg-count 1)
                        (format "Argument for '%s': " command-name)
                      (format "Argument %d/%d for '%s': " (1+ i) arg-count command-name))))
        (push (read-string prompt) args)))
    (nreverse args)))

;;; Common command file functions

(defun claude-code-emacs-list-command-files (directory)
  "List all .md files in DIRECTORY."
  (when (file-directory-p directory)
    (directory-files directory nil "\\.md$")))

(defun claude-code-emacs-read-command-file (filepath)
  "Read and trim the contents of command file at FILEPATH."
  (when (file-exists-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (string-trim (buffer-string)))))

;;; Custom project command functions

(defun claude-code-emacs-custom-commands-directory ()
  "Return the path to the .claude/commands directory for custom project commands."
  (let ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root))))
    (expand-file-name ".claude/commands" project-root)))

(defun claude-code-emacs-list-custom-command-files ()
  "List all .md files containing custom project commands.
Files are located in the .claude/commands directory."
  (claude-code-emacs-list-command-files
   (claude-code-emacs-custom-commands-directory)))

(defun claude-code-emacs-read-custom-command-file (filename)
  "Read the contents of a custom project command file FILENAME."
  (claude-code-emacs-read-command-file
   (expand-file-name filename (claude-code-emacs-custom-commands-directory))))

(defun claude-code-emacs-execute-custom-command ()
  "Select and execute a custom project command from .claude/commands.
If the command contains $ARGUMENTS, prompt for each argument and
send as /project:command args."
  (interactive)
  (let ((command-files (claude-code-emacs-list-custom-command-files)))
    (if command-files
        (let* ((selected-file (completing-read "Select custom command: " command-files nil t))
               (command-content (claude-code-emacs-read-custom-command-file selected-file)))
          (if command-content
              (let ((arg-count (claude-code-emacs-count-arguments command-content)))
                (if (> arg-count 0)
                    ;; Command contains $ARGUMENTS, prompt for arguments
                    (let ((args (claude-code-emacs-prompt-for-arguments
                                 (file-name-sans-extension selected-file) arg-count)))
                      (if (seq-some #'string-empty-p args)
                          (message "All arguments are required for this command")
                        ;; Send as /project:command arg1 arg2 ...
                        (claude-code-emacs-send-string
                         (format "/project:%s %s"
                                 (file-name-sans-extension selected-file)
                                 (mapconcat #'identity args " ")))))
                  ;; No $ARGUMENTS, send command content as is
                  (claude-code-emacs-send-string command-content)))
            (message "Failed to read custom command file: %s" selected-file)))
      (message "No custom command files found in %s" (claude-code-emacs-custom-commands-directory)))))

;;; Global command functions (from ~/.claude/commands)

(defun claude-code-emacs-global-commands-directory ()
  "Return the path to the ~/.claude/commands directory for global commands."
  (expand-file-name "~/.claude/commands"))

(defun claude-code-emacs-list-global-command-files ()
  "List all .md files in the ~/.claude/commands directory."
  (claude-code-emacs-list-command-files
   (claude-code-emacs-global-commands-directory)))

(defun claude-code-emacs-read-global-command-file (filename)
  "Read the contents of a global command file FILENAME."
  (claude-code-emacs-read-command-file
   (expand-file-name filename (claude-code-emacs-global-commands-directory))))

(defun claude-code-emacs-execute-global-command ()
  "Select and execute a global command from ~/.claude/commands using /user: prefix.
If the command file contains $ARGUMENTS, prompt for each argument."
  (interactive)
  (let ((command-files (claude-code-emacs-list-global-command-files)))
    (if command-files
        (let* ((selected-file (completing-read "Select global command: " command-files nil t))
               (file-content (claude-code-emacs-read-global-command-file selected-file))
               (arg-count (if file-content
                              (claude-code-emacs-count-arguments file-content)
                            0)))
          (if (> arg-count 0)
              ;; File contains $ARGUMENTS, prompt for arguments
              (let ((args (claude-code-emacs-prompt-for-arguments selected-file arg-count)))
                (if (seq-some #'string-empty-p args)
                    (message "All arguments are required for this command")
                  (claude-code-emacs-send-string
                   (format "/user:%s %s"
                           (file-name-sans-extension selected-file)
                           (mapconcat #'identity args " ")))))
            ;; No $ARGUMENTS, send as before
            (claude-code-emacs-send-string
             (format "/user:%s" (file-name-sans-extension selected-file)))))
      (message "No command files found in %s" (claude-code-emacs-global-commands-directory)))))

(provide 'claude-code-emacs-commands)
;;; claude-code-emacs-commands.el ends here
