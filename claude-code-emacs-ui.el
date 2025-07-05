;;; claude-code-emacs-ui.el --- UI components, modes, and transient menus for Claude Code Emacs -*- lexical-binding: t; -*-

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
;; - Major modes (claude-code-emacs-vterm-mode, claude-code-emacs-prompt-mode)
;; - Transient menus for command access
;; - File path completion and insertion
;; - Buffer management UI functions

;;; Code:

(require 'vterm)
(require 'transient)
(require 'projectile)
(require 'markdown-mode)

;; Forward declarations
(declare-function claude-code-emacs-run "claude-code-emacs-core" ())
(declare-function claude-code-emacs-switch-to-buffer "claude-code-emacs-core" ())
(declare-function claude-code-emacs-close "claude-code-emacs-core" ())
(declare-function claude-code-emacs-quit "claude-code-emacs-core" ())
(declare-function claude-code-emacs-send-region "claude-code-emacs-core" ())
(declare-function claude-code-emacs-send-string "claude-code-emacs-core" (string &optional paste-p))

;; Command forward declarations
(declare-function claude-code-emacs-send-1 "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-2 "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-3 "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-commit "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-push "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-escape "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-return "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-ctrl-r "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-ctrl-e "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-send-shift-tab "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-init "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-clear "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-help "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-execute-custom-command "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-memory "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-config "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-compact "claude-code-emacs-commands" (&optional instructions))
(declare-function claude-code-emacs-review "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-pr-comments "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-cost "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-status "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-login "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-logout "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-bug "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-doctor "claude-code-emacs-commands" ())
(declare-function claude-code-emacs-fix-diagnostic "claude-code-emacs-commands" ())

;; Prompt forward declarations
(declare-function claude-code-emacs-open-prompt-file "claude-code-emacs-prompt" ())
(declare-function claude-code-emacs-send-prompt-at-point "claude-code-emacs-prompt" ())
(declare-function claude-code-emacs-send-prompt-region "claude-code-emacs-prompt" ())
(declare-function claude-code-emacs-insert-region-path-to-prompt "claude-code-emacs-prompt" ())
(declare-function claude-code-emacs-insert-current-file-path-to-prompt "claude-code-emacs-prompt" ())
(declare-function claude-code-emacs-insert-current-file-path-to-session "claude-code-emacs-prompt" ())

;;; Major modes

(defvar claude-code-emacs-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Standard Emacs key bindings
    (define-key map (kbd "C-c C-q") 'claude-code-emacs-close)
    (define-key map (kbd "C-c C-g") 'claude-code-emacs-send-escape)
    (define-key map (kbd "C-c C-r") 'claude-code-emacs-send-ctrl-r)
    (define-key map (kbd "C-c C-e") 'claude-code-emacs-send-ctrl-e)
    (define-key map (kbd "C-c RET") 'claude-code-emacs-send-return)
    (define-key map (kbd "C-c TAB") 'claude-code-emacs-send-shift-tab)
    (define-key map (kbd "C-c C-t") 'claude-code-emacs-transient)
    map)
  "Keymap for `claude-code-emacs-vterm-mode'.")

(define-derived-mode claude-code-emacs-vterm-mode vterm-mode "Claude Code Session"
  "Major mode for Claude Code vterm sessions."
  (setq-local vterm-max-scrollback 500)
  (display-line-numbers-mode -1))


(defvar claude-code-emacs-prompt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-s") 'claude-code-emacs-send-prompt-at-point)
    (define-key map (kbd "C-c C-r") 'claude-code-emacs-send-prompt-region)
    (define-key map (kbd "C-c C-o") 'claude-code-emacs-run)
    (define-key map (kbd "C-c C-t") 'claude-code-emacs-prompt-transient)
    (define-key map "@" 'claude-code-emacs-self-insert-@)
    map)
  "Keymap for `claude-code-emacs-prompt-mode'.")

;;;###autoload
(define-derived-mode claude-code-emacs-prompt-mode markdown-mode "Claude Prompt"
  "Major mode for editing Claude Code prompt files.
\\{claude-code-emacs-prompt-mode-map}"
  (setq-local header-line-format
              '(:eval (format "Claude Code Prompts - %s"
                              (file-name-nondirectory (directory-file-name (claude-code-emacs-normalize-project-root (projectile-project-root)))))))
  (setq-local mode-line-format
              (append mode-line-format
                      '(" [C-c C-t: menu]")))
  ;; Add LSP language ID configuration if lsp-mode is available
  (when (and (require 'lsp-mode nil t)
             (boundp 'lsp-language-id-configuration))
    (add-to-list 'lsp-language-id-configuration
                 '(claude-code-emacs-prompt-mode . "markdown"))))

;;; File path completion and insertion

(defun claude-code-emacs-at-sign-complete ()
  "Complete file paths after @ symbol."
  (interactive)
  ;; NOTE: Don't use `claude-code-emacs-normalize-project-root' when passing project-root to projectile.el functions
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

(defun claude-code-emacs-self-insert-@ ()
  "Insert @ and trigger file completion."
  (interactive)
  (claude-code-emacs-at-sign-complete))

;;; Transient menus

;;;###autoload
(transient-define-prefix claude-code-emacs-transient ()
  "Claude Code Emacs main menu."
  ["Claude Code"
   ["Session"
    ("c" "Run Claude Code" claude-code-emacs-run)
    ("b" "Switch to Claude Code buffer" claude-code-emacs-switch-to-buffer)
    ("q" "Close Claude Code window" claude-code-emacs-close)
    ("Q" "Quit Claude Code session" claude-code-emacs-quit)
    ("p" "Open Prompt File" claude-code-emacs-open-prompt-file)]
   ["Actions"
    ("s" "Send menu" claude-code-emacs-send-transient)
    ("i" "Insert menu" claude-code-emacs-insert-transient)]
   ["Quick Send"
    ("1" "Send 1" claude-code-emacs-send-1)
    ("y" "Send 1 (yes)" claude-code-emacs-send-1)
    ("2" "Send 2" claude-code-emacs-send-2)
    ("3" "Send 3" claude-code-emacs-send-3)
    ("k" "Send Escape" claude-code-emacs-send-escape)
    ("m" "Send Return" claude-code-emacs-send-return)
    ("r" "Toggle expand (Ctrl+R)" claude-code-emacs-send-ctrl-r)
    ("e" "Toggle expand more (Ctrl+E)" claude-code-emacs-send-ctrl-e)
    ("a" "Toggle auto accept (Shift+Tab)" claude-code-emacs-send-shift-tab)]
   ["Commands"
    ("/" "Slash commands" claude-code-emacs-slash-commands-transient)
    ("x" "Execute custom command" claude-code-emacs-execute-custom-command)
    ("f" "Fix LSP diagnostic" claude-code-emacs-fix-diagnostic)]
   ["Git & GitHub"
    ("g" "Git & GitHub" claude-code-emacs-git-menu-transient)]
   ])

(transient-define-prefix claude-code-emacs-slash-commands-transient ()
  "Claude Code Emacs slash commands menu."
  ["Claude Code Slash Commands"
   ["Project & Session"
    ("i" "Init project (/init)" claude-code-emacs-init)
    ("k" "Clear conversation (/clear)" claude-code-emacs-clear)
    ("h" "Help (/help)" claude-code-emacs-help)]
   ["Memory & Config"
    ("m" "Memory (/memory)" claude-code-emacs-memory)
    ("c" "Config (/config)" claude-code-emacs-config)
    ("o" "Compact (/compact)" claude-code-emacs-compact)]
   ["Info & Status"
    ("$" "Cost (/cost)" claude-code-emacs-cost)
    ("s" "Status (/status)" claude-code-emacs-status)]
   ["Account"
    ("l" "Login (/login)" claude-code-emacs-login)
    ("L" "Logout (/logout)" claude-code-emacs-logout)]
   ["Other"
    ("b" "Report bug (/bug)" claude-code-emacs-bug)
    ("d" "Doctor (/doctor)" claude-code-emacs-doctor)]])

(transient-define-prefix claude-code-emacs-git-menu-transient ()
  "Claude Code Emacs git menu."
  ["Claude Code"
   ["Git"
    ("g" "Send commit" claude-code-emacs-send-commit)
    ("p" "Send push" claude-code-emacs-send-push)]
   ["GitHub"
    ("r" "Review" claude-code-emacs-review)
    ("c" "PR comments" claude-code-emacs-pr-comments)]])

(transient-define-prefix claude-code-emacs-send-transient ()
  "Claude Code Emacs send menu."
  ["Claude Code Send"
   [("s" "Send text" claude-code-emacs-send-string)]
   [("r" "Send region" claude-code-emacs-send-region)]])

(transient-define-prefix claude-code-emacs-insert-transient ()
  "Claude Code Emacs insert menu."
  ["Claude Code Insert"
   ["To Prompt Buffer"
    ("r" "Insert region and path" claude-code-emacs-insert-region-path-to-prompt)
    ("i" "Insert current file path" claude-code-emacs-insert-current-file-path-to-prompt)]
   ["To Session Buffer"
    ("I" "Insert current file path to session" claude-code-emacs-insert-current-file-path-to-session)]])

(transient-define-prefix claude-code-emacs-prompt-transient ()
  "Claude Code prompt buffer menu."
  ["Claude Code Prompt"
   ["Send"
    ("s" "Send section at point" claude-code-emacs-send-prompt-at-point)
    ("r" "Send region" claude-code-emacs-send-prompt-region)]
   ["Navigation"
    ("c" "Run Claude Code" claude-code-emacs-run)
    ("b" "Switch to Claude Code buffer" claude-code-emacs-switch-to-buffer)
    ("q" "Close Claude Code" claude-code-emacs-close)]])

;; Auto-mode for prompt files
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.claude-code-emacs\\.prompt\\.md\\'" . claude-code-emacs-prompt-mode))

(provide 'claude-code-emacs-ui)
;;; claude-code-emacs-ui.el ends here
