;;; claude-code-emacs.el --- Run Claude Code within Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  DESKTOP2

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "20241218.331") (transient "0.8.8") (projectile "2.9.1") (markdown-mode "2.7") (lsp-mode "9.0.0"))

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

;;

;;; Code:
(require 'vterm)
(require 'transient)
(require 'projectile)
(require 'markdown-mode)

;;;###autoload
(defun claude-code-emacs-run ()
  (interactive)
  (let* ((buffer-name (claude-code-emacs-buffer-name))
         (project-root (projectile-project-root))
         (default-directory project-root)
         (buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-code-emacs-vterm-mode)
        (claude-code-emacs-vterm-mode))
      ;; Wait for vterm to be ready
      (run-at-time 0.1 nil
                   (lambda ()
                     (when (buffer-live-p buf)
                       (with-current-buffer buf
                         (vterm-send-string "claude")
                         (vterm-send-return))))))
    (switch-to-buffer-other-window buffer-name)))

;;;###autoload
(defun claude-code-emacs-switch-to-buffer ()
  "Switch to the Claude Code buffer for the current project."
  (interactive)
  (let ((buffer-name (claude-code-emacs-buffer-name)))
    (if (get-buffer buffer-name)
        (switch-to-buffer-other-window buffer-name)
      (message "No Claude Code session for this project. Use 'claude-code-emacs-run' to start one."))))

(defun claude-code-emacs-buffer-name ()
  (let ((project-root (projectile-project-root)))
    (format "*claude:%s*" project-root)))

(defun claude-code-emacs-get-buffer ()
  "Get the Claude Code buffer for the current project, or nil if it doesn't exist."
  (get-buffer (claude-code-emacs-buffer-name)))

(defun claude-code-emacs-ensure-buffer ()
  "Ensure Claude Code buffer exists, error if not."
  (or (claude-code-emacs-get-buffer)
      (error "No Claude Code session for this project. Use 'claude-code-emacs-run' to start one")))

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

(defun claude-code-emacs-config (&optional args)
  (interactive "sConfig arguments (leave empty to show): ")
  (if args
      (if (string-empty-p args)
          (claude-code-emacs-send-string "/config")
        (claude-code-emacs-send-string (format "/config %s" args)))
    (claude-code-emacs-send-command-with-optional-args 
     "/config" "Config arguments (leave empty to show): ")))

(defun claude-code-emacs-compact (&optional instructions)
  (interactive "sCompact instructions (optional): ")
  (if instructions
      (if (string-empty-p instructions)
          (claude-code-emacs-send-string "/compact")
        (claude-code-emacs-send-string (format "/compact %s" instructions)))
    (claude-code-emacs-send-command-with-optional-args 
     "/compact" "Compact instructions (optional): ")))

(claude-code-emacs-define-slash-command "cost" "/cost")
(claude-code-emacs-define-slash-command "status" "/status")
(claude-code-emacs-define-slash-command "review" "/review")
(claude-code-emacs-define-slash-command "pr-comments" "/pr_comments")
(claude-code-emacs-define-slash-command "bug" "/bug")
(claude-code-emacs-define-slash-command "doctor" "/doctor")
(claude-code-emacs-define-slash-command "login" "/login")
(claude-code-emacs-define-slash-command "logout" "/logout")

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

;;; Key sending functions

(defun claude-code-emacs-with-vterm-buffer (body-fn)
  "Execute BODY-FN in the Claude Code vterm buffer."
  (let ((buf (claude-code-emacs-ensure-buffer)))
    (with-current-buffer buf
      (funcall body-fn))))

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

;; Since these need autoload cookies, we'll keep them as regular functions
;; but group them together for clarity

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
  "Send \='commit\=' to Claude Code buffer."
  (interactive)
  (claude-code-emacs-send-string "commit"))

;;;###autoload
(defun claude-code-emacs-send-ctrl-r ()
  "Send Ctrl+R to Claude Code buffer to toggle expand."
  (interactive)
  (claude-code-emacs-with-vterm-buffer 
   (lambda () (vterm-send-key (kbd "C-r")))))

(defun claude-code-emacs-chunk-string (str chunk-size)
  "Split STR into chunks of CHUNK-SIZE characters."
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
        (nreverse result)))))

(defun claude-code-emacs-send-string (string &optional paste-p)
  "Send STRING to Claude Code buffer. If PASTE-P is non-nil, paste the string."
  (let ((buf (claude-code-emacs-ensure-buffer)))
    (with-current-buffer buf
      (unless (eq major-mode 'claude-code-emacs-vterm-mode)
        (error "Buffer is not in claude-code-emacs-vterm-mode"))
      (let ((strings (claude-code-emacs-chunk-string string 50)))
        (mapc (lambda (str)
                (vterm-send-string str paste-p)
                (sleep-for 0.1))
              strings)
        (vterm-send-return)))))

;;;###autoload
(defun claude-code-emacs-send-region ()
  (interactive)
  (let ((text (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (buffer-substring-no-properties (point-min) (point-max)))))
    (claude-code-emacs-send-string text t)))

;;;###autoload
(defun claude-code-emacs-open-prompt-file ()
  "Open project-specific prompt file in another window."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (prompt-file (expand-file-name ".claude-code-emacs.prompt.md" project-root)))
    (unless (file-exists-p prompt-file)
      (with-temp-file prompt-file
        (insert "# Claude Code Prompts for " (file-name-nondirectory (directory-file-name project-root)) "\n\n"
                "This file contains prompts for Claude Code sessions in this project.\n\n"
                "## Project Context\n\n"
                "## Common Tasks\n\n"
                "## Code Patterns\n\n")))
    (switch-to-buffer-other-window (find-file-noselect prompt-file))
    (claude-code-emacs-prompt-mode)))

(define-derived-mode claude-code-emacs-vterm-mode vterm-mode "Claude Code Session"
  "Major mode for Claude Code vterm sessions."
  (display-line-numbers-mode -1))

(defun claude-code-emacs-send-prompt-at-point ()
  "Send the prompt section at point to Claude Code buffer."
  (interactive)
  (let ((section-text (claude-code-emacs-get-markdown-section-at-point)))
    (if section-text
        (claude-code-emacs-send-string section-text t)
      (message "No markdown section found at point"))))

(defun claude-code-emacs-send-prompt-region ()
  "Send the selected region from prompt buffer to Claude Code buffer."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (claude-code-emacs-send-string text t))
    (message "No region selected")))

(defun claude-code-emacs-get-markdown-section-at-point ()
  "Get the markdown section at point.
Returns text from current heading to next heading or end of buffer."
  (save-excursion
    (let* ((start (progn
                    (if (markdown-heading-at-point)
                        (point)
                      (markdown-previous-heading)
                      (point))))
           (end (progn
                  (goto-char start)
                  (if (markdown-next-heading)
                      (1- (point))
                    (point-max)))))
      (buffer-substring-no-properties start end))))

(defun claude-code-emacs-get-buffer-paths ()
  "Get all buffer file paths with project root replaced by @.
Returns a list of strings where project root is replaced with @ symbol.
Buffers without files or outside projects are excluded."
  (let ((project-root (projectile-project-root))
        (paths '()))
    (when project-root
      (dolist (buffer (buffer-list))
        (when-let ((file-path (buffer-file-name buffer)))
          (when (string-prefix-p project-root file-path)
            (let ((relative-path (file-relative-name file-path project-root)))
              (push (concat "@" relative-path) paths))))))
    (nreverse paths)))

(defun claude-code-emacs-format-buffer-path (buffer)
  "Format BUFFER's file path with project root replaced by @.
Returns nil if buffer has no file or is outside project."
  (when-let ((file-path (buffer-file-name buffer))
             (project-root (projectile-project-root)))
    (when (string-prefix-p project-root file-path)
      (let ((relative-path (file-relative-name file-path project-root)))
        (concat "@" relative-path)))))

(defun claude-code-emacs-insert-file-path ()
  "Insert a file path from current project buffers at point.
Presents a list of project files with @ prefix for selection."
  (interactive)
  (let* ((paths (claude-code-emacs-get-buffer-paths))
         (selected (when paths
                     (completing-read "Insert file path: " paths nil t))))
    (when selected
      (insert selected))))

(defun claude-code-emacs-insert-open-buffer-paths ()
  "Insert all open buffer file paths at point.
Each path is inserted on a new line with @ prefix."
  (interactive)
  (let ((paths (claude-code-emacs-get-buffer-paths)))
    (if paths
        (insert (mapconcat 'identity paths "\n"))
      (message "No project files are currently open"))))

(defun claude-code-emacs-at-sign-complete ()
  "Complete file paths after @ symbol."
  (interactive)
  (let ((project-files (projectile-project-files (projectile-project-root))))
    (when project-files
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (if bounds (car bounds) (point)))
             (selected (completing-read "File: "
                                        project-files
                                        nil t)))
        (when selected
          (delete-region start (point))
          (insert "@" selected))))))

(defun claude-code-emacs-self-insert-@ ()
  "Insert @ and trigger file completion."
  (interactive)
  (claude-code-emacs-at-sign-complete))

(defvar claude-code-emacs-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'claude-code-emacs-send-prompt-at-point)
    (define-key map (kbd "C-c C-r") 'claude-code-emacs-send-prompt-region)
    (define-key map (kbd "C-c C-o") 'claude-code-emacs-run)
    (define-key map (kbd "C-c C-t") 'claude-code-emacs-prompt-transient)
    (define-key map (kbd "C-c C-i") 'claude-code-emacs-insert-file-path)
    (define-key map (kbd "C-c C-a") 'claude-code-emacs-insert-open-buffer-paths)
    (define-key map "@" 'claude-code-emacs-self-insert-@)
    map)
  "Keymap for `claude-code-emacs-prompt-mode'.")

;;;###autoload
(define-derived-mode claude-code-emacs-prompt-mode markdown-mode "Claude Prompt"
  "Major mode for editing Claude Code prompt files.
\\{claude-code-emacs-prompt-mode-map}"
  (setq-local header-line-format
              '(:eval (format "Claude Code Prompts - %s"
                              (file-name-nondirectory (directory-file-name (projectile-project-root))))))
  (setq-local mode-line-format
              (append mode-line-format
                      '(" [C-c C-t: menu, C-c C-i: insert file]")))
  ;; Add LSP language ID configuration if lsp-mode is available
  (when (and (require 'lsp-mode nil t)
             (boundp 'lsp-language-id-configuration))
    (add-to-list 'lsp-language-id-configuration
                 '(claude-code-emacs-prompt-mode . "markdown"))))

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

;;; Custom project command functions

(defun claude-code-emacs-custom-commands-directory ()
  "Return the path to the .claude/commands directory for custom project commands."
  (let ((project-root (projectile-project-root)))
    (expand-file-name ".claude/commands" project-root)))

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

(defun claude-code-emacs-list-custom-command-files ()
  "List all .md files containing custom project commands.
Files are located in the .claude/commands directory."
  (claude-code-emacs-list-command-files 
   (claude-code-emacs-custom-commands-directory)))

(defun claude-code-emacs-read-custom-command-file (filename)
  "Read the contents of a custom project command file."
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
  "Read the contents of a global command file."
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

;;; Transient menus

;;;###autoload
(transient-define-prefix claude-code-emacs-transient ()
  "Claude Code Emacs main menu."
  ["Claude Code"
   ["Session"
    ("c" "Run Claude Code" claude-code-emacs-run)
    ("b" "Switch to Claude Code buffer" claude-code-emacs-switch-to-buffer)
    ("q" "Close Claude Code" claude-code-emacs-close)
    ("p" "Open Prompt File" claude-code-emacs-open-prompt-file)
    ("s" "Send Region" claude-code-emacs-send-region)]
   ["Quick Send"
    ("1" "Send 1" claude-code-emacs-send-1)
    ("y" "Send 1 (yes)" claude-code-emacs-send-1)
    ("2" "Send 2" claude-code-emacs-send-2)
    ("3" "Send 3" claude-code-emacs-send-3)
    ("g" "Send commit" claude-code-emacs-send-commit)
    ("e" "Send Escape" claude-code-emacs-send-escape)
    ("m" "Send Return" claude-code-emacs-send-return)
    ("r" "Send Ctrl+R (toggle expand)" claude-code-emacs-send-ctrl-r)]
   ["Commands"
    ("i" "Init project" claude-code-emacs-init)
    ("k" "Clear conversation" claude-code-emacs-clear)
    ("h" "Help" claude-code-emacs-help)
    ("x" "Execute custom project command" claude-code-emacs-execute-custom-command)
    ("X" "Execute global command (/user:)" claude-code-emacs-execute-global-command)]
   ["Memory & Config"
    ("M" "Memory" claude-code-emacs-memory)
    ("C" "Config" claude-code-emacs-config)
    ("o" "Compact" claude-code-emacs-compact)]
   ["Review"
    ("R" "Review" claude-code-emacs-review)
    ("P" "PR comments" claude-code-emacs-pr-comments)]
   ["Info & Review"
    ("$" "Cost" claude-code-emacs-cost)
    ("S" "Status" claude-code-emacs-status)]
   ["Account"
    ("l" "Login" claude-code-emacs-login)
    ("L" "Logout" claude-code-emacs-logout)]
   ["Other"
    ("B" "Report bug" claude-code-emacs-bug)
    ("D" "Doctor" claude-code-emacs-doctor)]
   ])

(transient-define-prefix claude-code-emacs-prompt-transient ()
  "Claude Code prompt buffer menu."
  ["Claude Code Prompt"
   ["Send"
    ("s" "Send section at point" claude-code-emacs-send-prompt-at-point)
    ("r" "Send region" claude-code-emacs-send-prompt-region)]
   ["Insert"
    ("i" "Insert file path" claude-code-emacs-insert-file-path)
    ("a" "Insert open buffer paths" claude-code-emacs-insert-open-buffer-paths)]
   ["Navigation"
    ("c" "Run Claude Code" claude-code-emacs-run)
    ("b" "Switch to Claude Code buffer" claude-code-emacs-switch-to-buffer)
    ("q" "Close Claude Code" claude-code-emacs-close)]])

;; Auto-mode for prompt files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.claude-code-emacs\\.prompt\\.md\\'" . claude-code-emacs-prompt-mode))

(provide 'claude-code-emacs)
;;; claude-code-emacs.el ends here
