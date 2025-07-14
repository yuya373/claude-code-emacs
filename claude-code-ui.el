;;; claude-code-ui.el --- UI components, modes, and transient menus for Claude Code Emacs -*- lexical-binding: t; -*-

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

;; This module provides UI components for Claude Code Emacs including:
;; - Major modes (claude-code-vterm-mode, claude-code-prompt-mode)
;; - Transient menus for command access
;; - File path completion and insertion
;; - Buffer management UI functions

;;; Code:

(require 'transient)
(require 'projectile)
(require 'markdown-mode)

(declare-function vterm-mode "vterm" ())

;; Forward declarations
(declare-function claude-code-run "claude-code-core" ())
(declare-function claude-code-switch-to-buffer "claude-code-core" ())
(declare-function claude-code-close "claude-code-core" ())
(declare-function claude-code-quit "claude-code-core" ())
(declare-function claude-code-send-region "claude-code-core" ())
(declare-function claude-code-send-string "claude-code-core" (string &optional paste-p))

;; Command forward declarations
(declare-function claude-code-send-1 "claude-code-commands" ())
(declare-function claude-code-send-2 "claude-code-commands" ())
(declare-function claude-code-send-3 "claude-code-commands" ())
(declare-function claude-code-send-commit "claude-code-commands" ())
(declare-function claude-code-send-push "claude-code-commands" ())
(declare-function claude-code-send-escape "claude-code-commands" ())
(declare-function claude-code-send-return "claude-code-commands" ())
(declare-function claude-code-send-ctrl-r "claude-code-commands" ())
(declare-function claude-code-send-ctrl-e "claude-code-commands" ())
(declare-function claude-code-send-shift-tab "claude-code-commands" ())
(declare-function claude-code-init "claude-code-commands" ())
(declare-function claude-code-clear "claude-code-commands" ())
(declare-function claude-code-help "claude-code-commands" ())
(declare-function claude-code-execute-custom-command "claude-code-commands" ())
(declare-function claude-code-memory "claude-code-commands" ())
(declare-function claude-code-config "claude-code-commands" ())
(declare-function claude-code-compact "claude-code-commands" (&optional instructions))
(declare-function claude-code-review "claude-code-commands" ())
(declare-function claude-code-pr-comments "claude-code-commands" ())
(declare-function claude-code-cost "claude-code-commands" ())
(declare-function claude-code-status "claude-code-commands" ())
(declare-function claude-code-login "claude-code-commands" ())
(declare-function claude-code-logout "claude-code-commands" ())
(declare-function claude-code-bug "claude-code-commands" ())
(declare-function claude-code-doctor "claude-code-commands" ())
(declare-function claude-code-fix-diagnostic "claude-code-commands" ())

;; Prompt forward declarations
(declare-function claude-code-open-prompt-file "claude-code-prompt" ())
(declare-function claude-code-send-prompt-at-point "claude-code-prompt" ())
(declare-function claude-code-send-prompt-region "claude-code-prompt" ())
(declare-function claude-code-insert-region-path-to-prompt "claude-code-prompt" ())
(declare-function claude-code-insert-current-file-path-to-prompt "claude-code-prompt" ())
(declare-function claude-code-insert-current-file-path-to-session "claude-code-prompt" ())

;;; Major modes

(defvar claude-code-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Standard Emacs key bindings
    (define-key map (kbd "C-c C-q") 'claude-code-close)
    (define-key map (kbd "C-c C-k") 'claude-code-send-escape)
    (define-key map (kbd "C-c C-r") 'claude-code-send-ctrl-r)
    (define-key map (kbd "C-c C-e") 'claude-code-send-ctrl-e)
    (define-key map (kbd "C-c RET") 'claude-code-send-return)
    (define-key map (kbd "C-c TAB") 'claude-code-send-shift-tab)
    (define-key map (kbd "C-c C-t") 'claude-code-transient)
    map)
  "Keymap for `claude-code-vterm-mode'.")

(define-derived-mode claude-code-vterm-mode vterm-mode "Claude Code Session"
  "Major mode for Claude Code vterm sessions."
  (setq-local vterm-max-scrollback 500)
  (display-line-numbers-mode -1))


(defvar claude-code-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'claude-code-send-prompt-at-point)
    (define-key map (kbd "C-c C-r") 'claude-code-send-prompt-region)
    (define-key map (kbd "C-c C-o") 'claude-code-run)
    (define-key map (kbd "C-c C-t") 'claude-code-prompt-transient)
    (define-key map "@" 'claude-code-self-insert-@)
    map)
  "Keymap for `claude-code-prompt-mode'.")

;;;###autoload
(define-derived-mode claude-code-prompt-mode markdown-mode "Claude Prompt"
  "Major mode for editing Claude Code prompt files.
\\{claude-code-prompt-mode-map}"
  (setq-local header-line-format
              '(:eval (format "Claude Code Prompts - %s"
                              (file-name-nondirectory (directory-file-name (claude-code-normalize-project-root (projectile-project-root)))))))
  (setq-local mode-line-format
              (append mode-line-format
                      '(" [C-c C-t: menu]")))
  ;; Add LSP language ID configuration if lsp-mode is available
  (when (and (require 'lsp-mode nil t)
             (boundp 'lsp-language-id-configuration))
    (add-to-list 'lsp-language-id-configuration
                 '(claude-code-prompt-mode . "markdown"))))

;;; File path completion and insertion

(defun claude-code-at-sign-complete ()
  "Complete file paths after @ symbol."
  (interactive)
  ;; NOTE: Don't use `claude-code-normalize-project-root' when passing project-root to projectile.el functions
  (let* ((project-files (projectile-project-files
                         (projectile-project-root))))
    (when project-files
      (let* ((selected (completing-read "File: "
                                        project-files
                                        nil nil)))
        (if selected
          ;; Check if there's already an @ before point
          (if (and (> (point) 1)
                   (eq (char-before) ?@))
              (insert selected)
            (insert "@" selected))
          (insert "@"))))))

(defun claude-code-self-insert-@ ()
  "Insert @ and trigger file completion."
  (interactive)
  (claude-code-at-sign-complete))

;;; Transient menus

;;;###autoload
(transient-define-prefix claude-code-transient ()
  "Claude Code Emacs main menu."
  ["Claude Code"
   ["Session"
    ("c" "Run Claude Code" claude-code-run)
    ("b" "Switch to Claude Code buffer" claude-code-switch-to-buffer)
    ("q" "Close Claude Code window" claude-code-close)
    ("Q" "Quit Claude Code session" claude-code-quit)
    ("p" "Open Prompt File" claude-code-open-prompt-file)]
   ["Actions"
    ("s" "Send menu" claude-code-send-transient)
    ("i" "Insert menu" claude-code-insert-transient)]
   ["Quick Send"
    ("1" "Send 1" claude-code-send-1)
    ("y" "Send 1 (yes)" claude-code-send-1)
    ("2" "Send 2" claude-code-send-2)
    ("3" "Send 3" claude-code-send-3)
    ("k" "Send Escape" claude-code-send-escape)
    ("m" "Send Return" claude-code-send-return)
    ("r" "Toggle expand (Ctrl+R)" claude-code-send-ctrl-r)
    ("e" "Toggle expand more (Ctrl+E)" claude-code-send-ctrl-e)
    ("a" "Toggle auto accept (Shift+Tab)" claude-code-send-shift-tab)]
   ["Commands"
    ("/" "Slash commands" claude-code-slash-commands-transient)
    ("x" "Execute custom command" claude-code-execute-custom-command)
    ("f" "Fix LSP diagnostic" claude-code-fix-diagnostic)]
   ["Git & GitHub"
    ("g" "Git & GitHub" claude-code-git-menu-transient)]
   ])

(transient-define-prefix claude-code-slash-commands-transient ()
  "Claude Code Emacs slash commands menu."
  ["Claude Code Slash Commands"
   ["Project & Session"
    ("i" "Init project (/init)" claude-code-init)
    ("k" "Clear conversation (/clear)" claude-code-clear)
    ("h" "Help (/help)" claude-code-help)]
   ["Memory & Config"
    ("m" "Memory (/memory)" claude-code-memory)
    ("c" "Config (/config)" claude-code-config)
    ("o" "Compact (/compact)" claude-code-compact)]
   ["Info & Status"
    ("$" "Cost (/cost)" claude-code-cost)
    ("s" "Status (/status)" claude-code-status)]
   ["Account"
    ("l" "Login (/login)" claude-code-login)
    ("L" "Logout (/logout)" claude-code-logout)]
   ["Other"
    ("b" "Report bug (/bug)" claude-code-bug)
    ("d" "Doctor (/doctor)" claude-code-doctor)]])

(transient-define-prefix claude-code-git-menu-transient ()
  "Claude Code Emacs git menu."
  ["Claude Code"
   ["Git"
    ("g" "Send commit" claude-code-send-commit)
    ("p" "Send push" claude-code-send-push)]
   ["GitHub"
    ("r" "Review" claude-code-review)
    ("c" "PR comments" claude-code-pr-comments)]])

(transient-define-prefix claude-code-send-transient ()
  "Claude Code Emacs send menu."
  ["Claude Code Send"
   [("s" "Send text" claude-code-send-string)]
   [("r" "Send region" claude-code-send-region)]])

(transient-define-prefix claude-code-insert-transient ()
  "Claude Code Emacs insert menu."
  ["Claude Code Insert"
   ["To Prompt Buffer"
    ("r" "Insert region and path" claude-code-insert-region-path-to-prompt)
    ("i" "Insert current file path" claude-code-insert-current-file-path-to-prompt)]
   ["To Session Buffer"
    ("I" "Insert current file path to session" claude-code-insert-current-file-path-to-session)]])

(transient-define-prefix claude-code-prompt-transient ()
  "Claude Code prompt buffer menu."
  ["Claude Code Prompt"
   ["Send"
    ("s" "Send section at point" claude-code-send-prompt-at-point)
    ("r" "Send region" claude-code-send-prompt-region)]
   ["Navigation"
    ("c" "Run Claude Code" claude-code-run)
    ("b" "Switch to Claude Code buffer" claude-code-switch-to-buffer)
    ("q" "Close Claude Code" claude-code-close)]])

;; Auto-mode for prompt files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.claude-code\\.prompt\\.md\\'" . claude-code-prompt-mode))

(provide 'claude-code-ui)
;;; claude-code-ui.el ends here
