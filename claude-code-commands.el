;;; claude-code-commands.el --- Slash commands and custom commands for Claude Code Emacs -*- lexical-binding: t; -*-

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

;; This module provides slash command definitions and custom command execution
;; functionality for Claude Code Emacs.  It includes:
;; - Built-in slash commands (/init, /clear, /help, etc.)
;; - Custom project commands from .claude/commands/*.md
;; - Global commands from ~/.claude/commands/*.md

;;; Code:

(require 'projectile)

;; Forward declarations
(declare-function claude-code-send-string "claude-code-core" (string &optional paste-p))
(declare-function claude-code-ensure-buffer "claude-code-core" ())
(declare-function claude-code-with-vterm-buffer "claude-code-core" (body-fn))
(declare-function claude-code-normalize-project-root "claude-code-core" (root))

;; vterm function declarations (vterm is loaded by core)
(declare-function vterm-send-escape "vterm" ())
(declare-function vterm-send-return "vterm" ())
(declare-function vterm-send-key "vterm" (key &optional shift))

;; LSP function declarations (optional dependency)
(declare-function lsp-diagnostics "lsp-mode" (&optional all-workspaces))
(declare-function lsp:diagnostic-range "lsp-protocol" (diagnostic))
(declare-function lsp:range-start "lsp-protocol" (range))
(declare-function lsp:position-line "lsp-protocol" (position))
(declare-function lsp:diagnostic-severity? "lsp-protocol" (diagnostic))
(declare-function lsp:diagnostic-message "lsp-protocol" (diagnostic))

;;; Slash command definitions

(defmacro claude-code-define-slash-command (name command)
  "Define a function claude-code-NAME that sends COMMAND."
  `(defun ,(intern (format "claude-code-%s" name)) ()
     (interactive)
     (claude-code-send-string ,command)))

;; Helper for commands with optional arguments
(defun claude-code-send-command-with-optional-args (command prompt)
  "Send COMMAND with optional arguments prompted by PROMPT."
  (let ((args (read-string prompt)))
    (if (string-empty-p args)
        (claude-code-send-string command)
      (claude-code-send-string (format "%s %s" command args)))))

;; Define simple slash commands
(claude-code-define-slash-command "init" "/init")
(claude-code-define-slash-command "clear" "/clear")
(claude-code-define-slash-command "help" "/help")
(claude-code-define-slash-command "memory" "/memory")
(claude-code-define-slash-command "config" "/config")
(claude-code-define-slash-command "cost" "/cost")
(claude-code-define-slash-command "status" "/status")
(claude-code-define-slash-command "review" "/review")
(claude-code-define-slash-command "pr-comments" "/pr_comments")
(claude-code-define-slash-command "bug" "/bug")
(claude-code-define-slash-command "doctor" "/doctor")
(claude-code-define-slash-command "login" "/login")
(claude-code-define-slash-command "logout" "/logout")

(defun claude-code-compact (&optional instructions)
  "Send /compact command with optional INSTRUCTIONS."
  (interactive "sCompact instructions (optional): ")
  (if instructions
      (if (string-empty-p instructions)
          (claude-code-send-string "/compact")
        (claude-code-send-string (format "/compact %s" instructions)))
    (claude-code-send-command-with-optional-args
     "/compact" "Compact instructions (optional): ")))

;;; Key sending functions

;;;###autoload
(defun claude-code-send-escape ()
  "Send ESC key to Claude Code buffer."
  (interactive)
  (claude-code-with-vterm-buffer #'vterm-send-escape))

;;;###autoload
(defun claude-code-send-return ()
  "Send Return key to Claude Code buffer."
  (interactive)
  (claude-code-with-vterm-buffer #'vterm-send-return))

;;; Quick send functions

;;;###autoload
(defun claude-code-send-1 ()
  "Send '1' to Claude Code buffer."
  (interactive)
  (claude-code-send-string "1"))

;;;###autoload
(defun claude-code-send-2 ()
  "Send '2' to Claude Code buffer."
  (interactive)
  (claude-code-send-string "2"))

;;;###autoload
(defun claude-code-send-3 ()
  "Send '3' to Claude Code buffer."
  (interactive)
  (claude-code-send-string "3"))

;;;###autoload
(defun claude-code-send-commit ()
  "Send `commit' to Claude Code buffer."
  (interactive)
  (claude-code-send-string "commit"))

;;;###autoload
(defun claude-code-send-push ()
  "Send `push' to Claude Code buffer."
  (interactive)
  (claude-code-send-string "push"))

;;;###autoload
(defun claude-code-send-ctrl-e ()
  "Send Ctrl+E to Claude Code buffer to toggle expand more."
  (interactive)
  (claude-code-with-vterm-buffer
   (lambda () (vterm-send-key (kbd "C-e")))))

;;;###autoload
(defun claude-code-send-ctrl-r ()
  "Send Ctrl+R to Claude Code buffer to toggle expand."
  (interactive)
  (claude-code-with-vterm-buffer
   (lambda () (vterm-send-key (kbd "C-r")))))

;;;###autoload
(defun claude-code-send-shift-tab ()
  "Send Shift+Tab to Claude Code buffer to toggle auto accept."
  (interactive)
  (claude-code-with-vterm-buffer
   (lambda ()
     (vterm-send-key "<tab>" t))))

;;; Helper functions for command argument handling

(defun claude-code-count-arguments (template)
  "Count the number of $ARGUMENTS placeholders in TEMPLATE."
  (let ((count 0)
        (pos 0))
    (while (string-match "\\$ARGUMENTS" template pos)
      (setq count (1+ count)
            pos (match-end 0)))
    count))

(defun claude-code-prompt-for-arguments (command-name)
  "Prompt user for arguments for COMMAND-NAME.
Returns a list with a single argument."
  (let* ((prompt (format "Argument for '%s': " command-name))
         (arg (read-string prompt)))
    (list arg)))

;;; Common command file functions

(defun claude-code-list-command-files (directory)
  "List all .md files in DIRECTORY."
  (when (file-directory-p directory)
    (directory-files directory nil "\\.md$")))

(defun claude-code-read-command-file (filepath)
  "Read and trim the contents of command file at FILEPATH."
  (when (file-exists-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath)
      (string-trim (buffer-string)))))

;;; Unified command selection with prefix

(defun claude-code-get-custom-commands ()
  "Get all available custom commands with appropriate prefixes.
Returns an alist of (display-name . command-info) where command-info
contains the type (project/user), filename, and directory."
  (let ((commands '()))
    ;; Add project commands
    (let ((project-files (claude-code-list-custom-command-files)))
      (dolist (file project-files)
        (let ((display-name (format "project:%s" (file-name-sans-extension file))))
          (push (cons display-name
                      `((type . project)
                        (filename . ,file)
                        (directory . ,(claude-code-custom-commands-directory))))
                commands))))

    ;; Add user commands
    (let ((user-files (claude-code-list-global-command-files)))
      (dolist (file user-files)
        (let ((display-name (format "user:%s" (file-name-sans-extension file))))
          (push (cons display-name
                      `((type . user)
                        (filename . ,file)
                        (directory . ,(claude-code-global-commands-directory))))
                commands))))

    (nreverse commands)))

(defun claude-code-execute-custom-command ()
  "Select and execute a custom command from both project and user commands.
Commands are displayed with \\='project:\\=' or \\='user:\\=' prefix for clarity,
but sent to Claude Code as plain command names (e.g., /command-name)."
  (interactive)
  (let ((commands (claude-code-get-custom-commands)))
    (if commands
        (let* ((selected (completing-read "Select command: "
                                          (mapcar #'car commands)
                                          nil t))
               (command-info (cdr (assoc selected commands)))
               (filename (cdr (assoc 'filename command-info)))
               (directory (cdr (assoc 'directory command-info)))
               (filepath (expand-file-name filename directory))
               (content (claude-code-read-command-file filepath)))
          (if content
              (let ((arg-count (claude-code-count-arguments content)))
                (if (> arg-count 0)
                    ;; Command contains $ARGUMENTS, prompt for arguments
                    (let ((args (claude-code-prompt-for-arguments
                                 (file-name-sans-extension filename))))
                      (if (seq-some #'string-empty-p args)
                          (message "All arguments are required for this command")
                        ;; Send with appropriate prefix
                        (claude-code-send-string
                         (format "/%s %s"
                                 (file-name-sans-extension filename)
                                 (mapconcat #'identity args " ")))))
                  ;; No $ARGUMENTS
                  (claude-code-send-string
                   (format "/%s"
                           (file-name-sans-extension filename)))))
            (message "Failed to read command file: %s" filename)))
      (message "No custom commands found"))))

;;; Custom project command functions

(defun claude-code-custom-commands-directory ()
  "Return the path to the .claude/commands directory for custom project commands."
  (let ((project-root (claude-code-normalize-project-root (projectile-project-root))))
    (expand-file-name ".claude/commands" project-root)))

(defun claude-code-list-custom-command-files ()
  "List all .md files containing custom project commands.
Files are located in the .claude/commands directory."
  (claude-code-list-command-files
   (claude-code-custom-commands-directory)))

(defun claude-code-read-custom-command-file (filename)
  "Read the contents of a custom project command file FILENAME."
  (claude-code-read-command-file
   (expand-file-name filename (claude-code-custom-commands-directory))))


;;; Global command functions (from ~/.claude/commands)

(defun claude-code-global-commands-directory ()
  "Return the path to the ~/.claude/commands directory for global commands."
  (expand-file-name "~/.claude/commands"))

(defun claude-code-list-global-command-files ()
  "List all .md files in the ~/.claude/commands directory."
  (claude-code-list-command-files
   (claude-code-global-commands-directory)))

(defun claude-code-read-global-command-file (filename)
  "Read the contents of a global command file FILENAME."
  (claude-code-read-command-file
   (expand-file-name filename (claude-code-global-commands-directory))))

;;; LSP diagnostics fix functions

;;;###autoload
(defun claude-code-fix-diagnostic ()
  "Select a diagnostic from `lsp-diagnostics' and send a fix prompt to Claude Code."
  (interactive)
  (unless (require 'lsp-mode nil t)
    (user-error "LSP mode is not installed"))
  (unless (bound-and-true-p lsp-mode)
    (user-error "LSP mode is not active in current buffer"))
  (require 'lsp-protocol)
  (let* ((diagnostics (lsp-diagnostics))
         (all-items '()))
    ;; Collect all diagnostics from all files
    (maphash (lambda (file diags)
               (dolist (diag diags)
                 (let* ((range (lsp:diagnostic-range diag))
                        (start (lsp:range-start range))
                        (line (1+ (lsp:position-line start)))
                        (severity (lsp:diagnostic-severity? diag))
                        (message (lsp:diagnostic-message diag))
                        (severity-str (pcase severity
                                        (1 "ERROR")
                                        (2 "WARNING")
                                        (3 "INFO")
                                        (4 "HINT")
                                        (_ "UNKNOWN")))
                        (display (format "[%s] %s:%d - %s"
                                         severity-str
                                         (file-relative-name file (projectile-project-root))
                                         line
                                         message)))
                   (push (list display file line message severity-str) all-items))))
             diagnostics)

    (if (null all-items)
        (message "No diagnostics found")
      ;; Sort by severity (errors first) and then by file/line
      (setq all-items (sort all-items
                            (lambda (a b)
                              (let ((sev-a (nth 4 a))
                                    (sev-b (nth 4 b)))
                                (if (string= sev-a sev-b)
                                    (or (string< (nth 1 a) (nth 1 b))
                                        (and (string= (nth 1 a) (nth 1 b))
                                             (< (nth 2 a) (nth 2 b))))
                                  (string< sev-a sev-b))))))

      (let* ((selected (completing-read "Select diagnostic to fix: "
                                        (mapcar #'car all-items)
                                        nil t))
             (item (cl-find selected all-items :key #'car :test #'string=)))
        (when item
          (let* ((file (nth 1 item))
                 (line (nth 2 item))
                 (message (nth 3 item))
                 (severity (nth 4 item))
                 (relative-path (file-relative-name file (projectile-project-root)))
                 (prompt (format "Fix the following %s in @%s at line %d:\n\n%s\n\nPlease fix this issue."
                                 (downcase severity)
                                 relative-path
                                 line
                                 message)))
            (claude-code-send-string prompt)))))))


(provide 'claude-code-commands)
;;; claude-code-commands.el ends here
