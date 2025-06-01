;;; claude-code-emacs.el --- Run Claude Code within Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025  DESKTOP2

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (vterm "20241218.331") (transient "0.8.8") (projectile "2.9.1") (markdown-mode "2.7"))

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

(defun claude-code-emacs-init ()
  (interactive)
  (claude-code-emacs-send-string "/init"))

(defun claude-code-emacs-clear ()
  (interactive)
  (claude-code-emacs-send-string "/clear"))

(defun claude-code-emacs-help ()
  (interactive)
  (claude-code-emacs-send-string "/help"))

(defun claude-code-emacs-config (&optional args)
  (interactive "sConfig arguments (leave empty to show): ")
  (if (string-empty-p args)
      (claude-code-emacs-send-string "/config")
    (claude-code-emacs-send-string (format "/config %s" args))))

(defun claude-code-emacs-memory ()
  (interactive)
  (claude-code-emacs-send-string "/memory"))

(defun claude-code-emacs-compact (&optional instructions)
  (interactive "sCompact instructions (optional): ")
  (if (string-empty-p instructions)
      (claude-code-emacs-send-string "/compact")
    (claude-code-emacs-send-string (format "/compact %s" instructions))))

(defun claude-code-emacs-cost ()
  (interactive)
  (claude-code-emacs-send-string "/cost"))

(defun claude-code-emacs-status ()
  (interactive)
  (claude-code-emacs-send-string "/status"))

(defun claude-code-emacs-review ()
  (interactive)
  (claude-code-emacs-send-string "/review"))

(defun claude-code-emacs-pr-comments ()
  (interactive)
  (claude-code-emacs-send-string "/pr_comments"))

(defun claude-code-emacs-bug ()
  (interactive)
  (claude-code-emacs-send-string "/bug"))

(defun claude-code-emacs-doctor ()
  (interactive)
  (claude-code-emacs-send-string "/doctor"))

(defun claude-code-emacs-login ()
  (interactive)
  (claude-code-emacs-send-string "/login"))

(defun claude-code-emacs-logout ()
  (interactive)
  (claude-code-emacs-send-string "/logout"))

;;;###autoload
(defun claude-code-emacs-send-escape ()
  "Send ESC key to Claude Code buffer."
  (interactive)
  (let ((buf (claude-code-emacs-ensure-buffer)))
    (with-current-buffer buf
      (vterm-send-escape))))

;;;###autoload
(defun claude-code-emacs-send-return ()
  "Send Return key to Claude Code buffer."
  (interactive)
  (let ((buf (claude-code-emacs-ensure-buffer)))
    (with-current-buffer buf
      (vterm-send-return))))

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
                (vterm-send-string str paste-p))
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

;;; Transient menus

;;;###autoload
(transient-define-prefix claude-code-emacs-transient ()
  "Claude Code Emacs main menu."
  ["Claude Code"
   ["Session"
    ("c" "Run Claude Code" claude-code-emacs-run)
    ("b" "Switch to Claude Code buffer" claude-code-emacs-switch-to-buffer)
    ("p" "Open Prompt File" claude-code-emacs-open-prompt-file)
    ("s" "Send Region" claude-code-emacs-send-region)]
   ["Quick Send"
    ("1" "Send 1" claude-code-emacs-send-1)
    ("y" "Send 1 (yes)" claude-code-emacs-send-1)
    ("2" "Send 2" claude-code-emacs-send-2)
    ("3" "Send 3" claude-code-emacs-send-3)
    ("g" "Send commit" claude-code-emacs-send-commit)
    ("e" "Send Escape" claude-code-emacs-send-escape)
    ("m" "Send Return" claude-code-emacs-send-return)]
   ["Commands"
    ("i" "Init project" claude-code-emacs-init)
    ("k" "Clear conversation" claude-code-emacs-clear)
    ("h" "Help" claude-code-emacs-help)]
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
    ("b" "Switch to Claude Code buffer" claude-code-emacs-switch-to-buffer)]])

;; Auto-mode for prompt files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.claude-code-emacs\\.prompt\\.md\\'" . claude-code-emacs-prompt-mode))

(provide 'claude-code-emacs)
;;; claude-code-emacs.el ends here
