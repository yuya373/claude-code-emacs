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
(declare-function claude-code-buffer-name "claude-code-core" ())
(declare-function claude-code-normalize-project-root "claude-code-core" (project-root))

;; Command forward declarations
(declare-function claude-code-send-1 "claude-code-commands" ())
(declare-function claude-code-send-2 "claude-code-commands" ())
(declare-function claude-code-send-3 "claude-code-commands" ())
(declare-function claude-code-send-commit "claude-code-commands" ())
(declare-function claude-code-send-push "claude-code-commands" ())
(declare-function claude-code-send-escape "claude-code-commands" ())
(declare-function claude-code-send-return "claude-code-commands" ())
(declare-function claude-code-send-ctrl-o "claude-code-commands" ())
(declare-function claude-code-send-ctrl-r "claude-code-commands" ())
(declare-function claude-code-send-ctrl-e "claude-code-commands" ())
(declare-function claude-code-send-shift-tab "claude-code-commands" ())
(declare-function claude-code-send-ctrl-t "claude-code-commands" ())
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

;;;;; Vterm terminal customizations
(defcustom claude-code-vterm-buffer-multiline-output t
  "Whether to buffer vterm output to prevent flickering on multi-line input.

When non-nil, vterm output that appears to be redrawing multi-line
input boxes will be buffered briefly and processed in a single
batch.  This prevents the flickering that can occur when Claude redraws
its input box as it expands to multiple lines.

This only affects the vterm backend."
  :type 'boolean
  :group 'claude-code-ui)

(defcustom claude-code-vterm-multiline-delay 0.016
  "Delay in seconds before processing buffered vterm output.

This controls how long vterm waits to collect output before processing
it when `claude-code-vterm-buffer-multiline-output' is enabled.
The delay should be long enough to collect bursts of updates but short
enough to not be noticeable to the user.

The default value of 0.016 seconds (60FPS) provides a good balance
between reducing flickering and maintaining responsiveness.

Minimum value is 0.001 seconds to ensure proper operation."
  :type 'number
  :set (lambda (symbol value)
         (if (and (numberp value) (>= value 0.001))
             (set-default symbol value)
           (error "Claude-code-vterm-multiline-delay must be at least 0.001 seconds")))
  :group 'claude-code-ui)

;;; Major modes

(defvar claude-code-vterm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Standard Emacs key bindings
    (define-key map (kbd "C-c C-q") 'claude-code-close)
    (define-key map (kbd "C-c C-k") 'claude-code-send-escape)
    (define-key map (kbd "C-c C-o") 'claude-code-send-ctrl-o)
    (define-key map (kbd "C-c C-e") 'claude-code-send-ctrl-e)
    (define-key map (kbd "C-c C-d") 'claude-code-send-ctrl-t) ; d for "display TODOs"
    (define-key map (kbd "C-c RET") 'claude-code-send-return)
    (define-key map (kbd "C-c TAB") 'claude-code-send-shift-tab)
    (define-key map (kbd "C-c C-t") 'claude-code-transient)
    map)
  "Keymap for `claude-code-vterm-mode'.")

(defvar-local claude-code--vterm-multiline-buffer nil
  "Buffer for accumulating multi-line vterm output.")

(defvar-local claude-code--vterm-multiline-buffer-timer nil
  "Timer for processing buffered multi-line vterm output.")

(defun claude-code--vterm-cleanup-multiline-timer ()
  "Clean up multiline buffer timer."
  (when claude-code--vterm-multiline-buffer-timer
    (cancel-timer claude-code--vterm-multiline-buffer-timer)
    (setq claude-code--vterm-multiline-buffer-timer nil))
  (setq claude-code--vterm-multiline-buffer nil))

(defun claude-code--vterm-multiline-buffer-filter (orig-fun process input)
  "Buffer vterm output when it appears to be redrawing multi-line input.
This prevents flickering when Claude redraws its input box as it expands
to multiple lines.  We detect this by looking for escape sequences that
indicate cursor positioning and line clearing operations.

ORIG-FUN is the original vterm--filter function.
PROCESS is the vterm process.
INPUT is the terminal output string."
  (if (or (not (stringp input))
          (not claude-code-vterm-buffer-multiline-output)
          (not (equal (claude-code-buffer-name)
                      (buffer-name (process-buffer process)))))
      ;; Feature disabled or not a Claude buffer, pass through normally
      (funcall orig-fun process input)
    (with-current-buffer (process-buffer process)
      ;; Check if this looks like multi-line input box redraw
      ;; Common patterns when redrawing multi-line input:
      ;; - ESC[K (clear to end of line)
      ;; - ESC[<n>;<m>H (cursor positioning)
      ;; - ESC[<n>A/B/C/D (cursor movement)
      ;; - Multiple of these in sequence
      (let ((has-clear-line (string-match-p "\033\\[K" input))
            (has-cursor-pos (string-match-p "\033\\[[0-9]+;[0-9]+H" input))
            (has-cursor-move (string-match-p "\033\\[[0-9]*[ABCD]" input))
            (escape-count (cl-count ?\033 input)))

        ;; If we see multiple escape sequences that look like redrawing,
        ;; or we're already buffering, add to buffer
        (if (or (and (>= escape-count 3)
                     (or has-clear-line has-cursor-pos has-cursor-move))
                claude-code--vterm-multiline-buffer)
            (progn
              (setq claude-code--vterm-multiline-buffer (concat claude-code--vterm-multiline-buffer input))
              ;; Debouncing `vterm--filter'
              (when claude-code--vterm-multiline-buffer-timer
                (cancel-timer claude-code--vterm-multiline-buffer-timer))
              (setq claude-code--vterm-multiline-buffer-timer
                    (run-at-time claude-code-vterm-multiline-delay nil
                                 (lambda (buf)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       (when claude-code--vterm-multiline-buffer
                                         (let ((inhibit-redisplay t)
                                               (data claude-code--vterm-multiline-buffer))
                                           ;; Clear buffer first to prevent recursion
                                           (setq claude-code--vterm-multiline-buffer nil
                                                 claude-code--vterm-multiline-buffer-timer nil)
                                           ;; Process all buffered data at once
                                           (when-let* ((proc (get-buffer-process buf)))
                                             (when (process-live-p proc)
                                               (condition-case err
                                                   (funcall orig-fun proc data)
                                                 (error
                                                  (message "Error in vterm filter: %s" err))))))))))
                                 (process-buffer process))))
          ;; Not multi-line redraw, process normally
          (funcall orig-fun process input))))))

(define-derived-mode claude-code-vterm-mode vterm-mode "Claude Code Session"
  "Major mode for Claude Code vterm sessions."
  (setq-local vterm-max-scrollback 500
              vterm-ignore-blink-cursor t
              ;; disable any built-in cursor management
              cursor-in-non-selected-windows nil
              blink-cursor-mode nil
              cursor-type nil
              ;; disable hl-line-mode
              hl-line-mode nil
              global-hl-line-mode nil)
  (hl-line-mode -1)
  (display-line-numbers-mode -1)
  (face-remap-add-relative 'nobreak-space '(:underline nil))
  ;; Clean up timer on buffer kill
  (add-hook 'kill-buffer-hook #'claude-code--vterm-cleanup-multiline-timer nil t)

  (when-let* ((proc (get-buffer-process (current-buffer)))
              (orig-fun (process-filter proc)))
    (set-process-filter
     proc
     (lambda (process input)
       (condition-case err
           (claude-code--vterm-multiline-buffer-filter orig-fun process input)
         (error
          (message "Error in Claude Code vterm filter: %s" err)
          ;; Pass through the input even if there's an error to avoid breaking the terminal
          (funcall orig-fun process input)))))))

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
    ("o" "Toggle expand (Ctrl+O)" claude-code-send-ctrl-o)
    ("e" "Toggle expand more (Ctrl+E)" claude-code-send-ctrl-e)
    ("t" "Toggle TODO display (Ctrl+T)" claude-code-send-ctrl-t)
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
    ("s" "Insert current file path to session" claude-code-insert-current-file-path-to-session)]])

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
