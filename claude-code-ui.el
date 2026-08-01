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
(declare-function face-remap-remove-relative "face-remap" (cookie))
(defvar vterm-copy-mode)

;; Forward declarations
(declare-function claude-code-run "claude-code-core" ())
(declare-function claude-code-switch-to-buffer "claude-code-core" ())
(declare-function claude-code-close "claude-code-core" ())
(declare-function claude-code-quit "claude-code-core" ())
(declare-function claude-code-send-region "claude-code-core" ())
(declare-function claude-code-send-string "claude-code-core" (string &optional paste-p))
(declare-function claude-code-buffer-name "claude-code-core" ())
(declare-function claude-code-normalize-project-root "claude-code-core" (project-root))
(declare-function claude-code-with-vterm-buffer "claude-code-core" (body-fn))

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
(declare-function claude-code-send-tab "claude-code-commands" ())
(declare-function claude-code-send-page-up "claude-code-commands" ())
(declare-function claude-code-send-page-down "claude-code-commands" ())
(declare-function claude-code-send-line-up "claude-code-commands" ())
(declare-function claude-code-send-line-down "claude-code-commands" ())
(declare-function claude-code-send-ctrl-end "claude-code-commands" ())
(declare-function claude-code-send-left "claude-code-commands" ())
(declare-function claude-code-send-up "claude-code-commands" ())
(declare-function claude-code-send-down "claude-code-commands" ())
(declare-function claude-code-send-ctrl-x "claude-code-commands" ())
(declare-function claude-code-send-ctrl-s "claude-code-commands" ())
(declare-function claude-code-send-meta-1 "claude-code-commands" ())
(declare-function claude-code-agent-view-rename "claude-code-commands" (name))
(declare-function claude-code-init "claude-code-commands" ())
(declare-function claude-code-clear "claude-code-commands" ())
(declare-function claude-code-help "claude-code-commands" ())
(declare-function claude-code-plan "claude-code-commands" ())
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
    (define-key map (kbd "C-c C-n") 'claude-code-send-tab) ; n for "thinking mode"
    (define-key map (kbd "C-c RET") 'claude-code-send-return)
    (define-key map (kbd "C-c TAB") 'claude-code-send-shift-tab)
    (define-key map (kbd "C-c C-t") 'claude-code-transient)
    (define-key map (kbd "C-c C-s") 'claude-code-vterm-scroll-mode) ; s for scroll
    (define-key map (kbd "C-c C-a") 'claude-code-vterm-agent-view) ; a for agents
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
  ;; NOTE: A process filter runs with whatever buffer happened to be
  ;; current when output arrived, not the process's own buffer.  We must
  ;; therefore resolve the expected Claude buffer name relative to the
  ;; process buffer, whose `default-directory' is the project root.
  ;; Otherwise `claude-code-buffer-name' evaluates `projectile-project-root'
  ;; against the ambient buffer and signals a `user-error' whenever that
  ;; buffer is outside a project, flooding *Messages* (see issue #17).
  (let ((claude-buffer-name
         (when-let* ((proc-buffer (process-buffer process))
                     ((buffer-live-p proc-buffer)))
           (with-current-buffer proc-buffer
             (ignore-errors (claude-code-buffer-name))))))
    (if (or (not (stringp input))
            (not claude-code-vterm-buffer-multiline-output)
            ;; Nil when the process buffer is dead or outside a project;
            ;; requiring non-nil keeps a dead buffer (whose `buffer-name'
            ;; is also nil) from matching the `equal' below.
            (not claude-buffer-name)
            (not (equal claude-buffer-name
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
            (funcall orig-fun process input)))))))

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
  ;; Restore Emacs' built-in cursor in `vterm-copy-mode'.  We disable
  ;; `cursor-type' above to reduce flicker since vterm draws its own
  ;; cursor, but vterm stops drawing it in copy-mode -- without this
  ;; hook, the cursor would be invisible while copying text.
  (add-hook 'vterm-copy-mode-hook
            (lambda ()
              (setq-local cursor-type (when vterm-copy-mode t)))
            nil t)
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

;;;; Shared helpers for vterm minor modes (scroll mode, agent view mode)

(defun claude-code--vterm-minor-mode-enable (highlight face cookie-sym)
  "Remap the mode line with FACE when HIGHLIGHT is non-nil.
Stores the remap cookie in the buffer-local variable named COOKIE-SYM so
`claude-code--vterm-minor-mode-disable' can undo the remap later."
  (when highlight
    (set cookie-sym (face-remap-add-relative 'mode-line face)))
  (force-mode-line-update))

(defun claude-code--vterm-minor-mode-disable (cookie-sym)
  "Undo the mode-line face remap stored in the buffer-local COOKIE-SYM."
  (when (symbol-value cookie-sym)
    (face-remap-remove-relative (symbol-value cookie-sym))
    (set cookie-sym nil))
  (force-mode-line-update))

;;;; Scroll minor mode (for Claude Code fullscreen mode)

(defface claude-code-vterm-scroll-mode-lighter-face
  '((((class color) (background dark))
     :background "DarkOrange3" :foreground "white" :weight bold)
    (((class color) (background light))
     :background "DarkOrange" :foreground "black" :weight bold)
    (t :inverse-video t :weight bold))
  "Face for the lighter shown in the mode line while scroll mode is active."
  :group 'claude-code-ui)

(defface claude-code-vterm-scroll-mode-line-face
  '((((class color) (background dark))
     :background "DarkOrange3" :foreground "white" :weight bold)
    (((class color) (background light))
     :background "DarkOrange" :foreground "black" :weight bold)
    (t :inverse-video t :weight bold))
  "Face used to remap the mode line while scroll mode is active.

When `claude-code-vterm-scroll-mode-highlight-modeline' is non-nil
the entire mode line of the Claude Code buffer takes on this face
while the mode is active."
  :group 'claude-code-ui)

(defcustom claude-code-vterm-scroll-mode-highlight-modeline t
  "Whether to remap the mode line face while scroll mode is active.

When non-nil, the entire mode line of the Claude Code buffer is
highlighted using `claude-code-vterm-scroll-mode-line-face' so the
active scroll mode is visually obvious."
  :type 'boolean
  :group 'claude-code-ui)

(defvar-local claude-code--vterm-scroll-mode-face-cookie nil
  "Cookie returned by `face-remap-add-relative' for the mode line.

Used to undo the mode-line face remap when scroll mode is disabled.")

(defvar claude-code-vterm-scroll-mode-lighter
  (propertize " 📜SCROLL "
              'face 'claude-code-vterm-scroll-mode-lighter-face)
  "Lighter shown in the mode line while scroll mode is active.

A propertized string with `claude-code-vterm-scroll-mode-lighter-face'
so the lighter is visually prominent.")

(defvar claude-code-vterm-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Page scroll
    (define-key map (kbd "<prior>") 'claude-code-send-page-up)
    (define-key map (kbd "<next>") 'claude-code-send-page-down)
    ;; Line scroll (Shift+arrow)
    (define-key map (kbd "S-<up>") 'claude-code-send-line-up)
    (define-key map (kbd "S-<down>") 'claude-code-send-line-down)
    ;; Jump to bottom
    (define-key map (kbd "C-<end>") 'claude-code-send-ctrl-end)
    ;; Toggle back to normal vterm input mode
    (define-key map (kbd "C-c C-s") 'claude-code-vterm-scroll-mode)
    (define-key map (kbd "q") 'claude-code-vterm-scroll-mode)
    map)
  "Keymap for `claude-code-vterm-scroll-mode'.")

;;;###autoload
(define-minor-mode claude-code-vterm-scroll-mode
  "Minor mode for scrolling Claude Code fullscreen output.

When enabled in a `claude-code-vterm-mode' buffer, this mode binds keys
for scrolling the Claude Code fullscreen view
\(see https://code.claude.com/docs/en/fullscreen\).

Keybindings:
\\{claude-code-vterm-scroll-mode-map}"
  :init-value nil
  :lighter claude-code-vterm-scroll-mode-lighter
  :keymap claude-code-vterm-scroll-mode-map
  (cond
   ;; Refuse to enable outside vterm buffers
   ((and claude-code-vterm-scroll-mode
         (not (derived-mode-p 'vterm-mode)))
    (claude-code-vterm-scroll-mode -1)
    (user-error "claude-code-vterm-scroll-mode is only available in vterm buffers"))
   ;; Enabling: remap the mode-line face for prominent visual feedback
   (claude-code-vterm-scroll-mode
    (claude-code--vterm-minor-mode-enable
     claude-code-vterm-scroll-mode-highlight-modeline
     'claude-code-vterm-scroll-mode-line-face
     'claude-code--vterm-scroll-mode-face-cookie))
   ;; Disabling: undo the mode-line face remap
   (t
    (claude-code--vterm-minor-mode-disable
     'claude-code--vterm-scroll-mode-face-cookie))))

;;;; Agent view minor mode (for Claude Code agents view)

(defface claude-code-vterm-agent-mode-lighter-face
  '((((class color) (background dark))
     :background "DeepSkyBlue4" :foreground "white" :weight bold)
    (((class color) (background light))
     :background "DeepSkyBlue" :foreground "black" :weight bold)
    (t :inverse-video t :weight bold))
  "Face for the lighter shown in the mode line while agent view mode is active."
  :group 'claude-code-ui)

(defface claude-code-vterm-agent-mode-line-face
  '((((class color) (background dark))
     :background "DeepSkyBlue4" :foreground "white" :weight bold)
    (((class color) (background light))
     :background "DeepSkyBlue" :foreground "black" :weight bold)
    (t :inverse-video t :weight bold))
  "Face used to remap the mode line while agent view mode is active.

When `claude-code-vterm-agent-mode-highlight-modeline' is non-nil
the entire mode line of the Claude Code buffer takes on this face
while the mode is active."
  :group 'claude-code-ui)

(defcustom claude-code-vterm-agent-mode-highlight-modeline t
  "Whether to remap the mode line face while agent view mode is active.

When non-nil, the entire mode line of the Claude Code buffer is
highlighted using `claude-code-vterm-agent-mode-line-face' so the
active agent view mode is visually obvious."
  :type 'boolean
  :group 'claude-code-ui)

(defvar-local claude-code--vterm-agent-mode-face-cookie nil
  "Cookie returned by `face-remap-add-relative' for the mode line.

Used to undo the mode-line face remap when agent view mode is disabled.")

(defvar claude-code-vterm-agent-mode-lighter
  (propertize " 🤖AGENTS "
              'face 'claude-code-vterm-agent-mode-lighter-face)
  "Lighter shown in the mode line while agent view mode is active.

A propertized string with `claude-code-vterm-agent-mode-lighter-face'
so the lighter is visually prominent.")

(defvar claude-code-vterm-agent-mode-map
  (let ((map (make-sparse-keymap)))
    ;; The agents view accepts free text input (e.g. composing a message
    ;; to an agent), so this mode must not intercept self-inserting
    ;; keys, RET, the arrows, or the TUI's own control keys (C-r, C-s,
    ;; C-t, ESC) -- vterm already passes them all through.  Mode
    ;; commands therefore live on the C-c prefix, plus M-1, which vterm
    ;; cannot pass through (it runs `digit-argument').
    (define-key map (kbd "C-c C-r") 'claude-code-agent-view-rename)
    (define-key map (kbd "C-c C-x") 'claude-code-vterm-agent-mode-stop)
    ;; Toggle the mode back off; keys pass through either way, so no
    ;; key is sent to the CLI
    (define-key map (kbd "C-c C-a") 'claude-code-vterm-agent-mode)
    ;; Help menu (C-c + punctuation is minor mode territory)
    (define-key map (kbd "C-c ?") 'claude-code-agent-view-transient)
    (define-key map (kbd "M-1") 'claude-code-vterm-agent-mode-open-alt)
    map)
  "Keymap for `claude-code-vterm-agent-mode'.")

;;;###autoload
(define-minor-mode claude-code-vterm-agent-mode
  "Minor mode for operating the Claude Code agents view.

The agents view accepts free text input (e.g. composing a message to
an agent), so this mode intercepts no self-inserting keys: typing,
RET, the arrows, and the TUI's own control keys (C-r, C-s, C-t, ESC)
all pass through to the terminal as usual.  The mode only adds a
mode-line indicator and a few commands on the C-c prefix.  Press
\\`C-c ?' for a menu of the available commands.
Use `claude-code-vterm-agent-view' to open the agents view and enable
this mode in one step.

Keybindings:
\\{claude-code-vterm-agent-mode-map}"
  :init-value nil
  :lighter claude-code-vterm-agent-mode-lighter
  :keymap claude-code-vterm-agent-mode-map
  (cond
   ;; Refuse to enable outside Claude Code session buffers: every key
   ;; command in this mode acts on the project's Claude buffer, so
   ;; enabling it in any other vterm buffer would silently redirect that
   ;; terminal's keys away from it.
   ((and claude-code-vterm-agent-mode
         (not (derived-mode-p 'claude-code-vterm-mode)))
    (claude-code-vterm-agent-mode -1)
    (user-error "claude-code-vterm-agent-mode is only available in Claude Code session buffers"))
   ;; Enabling: remap the mode-line face for prominent visual feedback
   (claude-code-vterm-agent-mode
    (claude-code--vterm-minor-mode-enable
     claude-code-vterm-agent-mode-highlight-modeline
     'claude-code-vterm-agent-mode-line-face
     'claude-code--vterm-agent-mode-face-cookie))
   ;; Disabling: undo the mode-line face remap
   (t
    (claude-code--vterm-minor-mode-disable
     'claude-code--vterm-agent-mode-face-cookie))))

;;;###autoload
(defun claude-code-vterm-agent-view ()
  "Open the Claude Code agents view and enable `claude-code-vterm-agent-mode'.
The mode is enabled in the project's Claude Code buffer, so this signals
an error when no session is running instead of leaving the current
buffer stuck in the mode.  Sends Left arrow to Claude Code to switch to
the agents view."
  (interactive)
  (claude-code-with-vterm-buffer
   (lambda () (claude-code-vterm-agent-mode 1)))
  (claude-code-send-left))

(defun claude-code-vterm-agent-mode-stop ()
  "Stop the selected agent after asking for confirmation.
Sends Ctrl+X to Claude Code; stopping kills the agent's in-progress
work, so the confirmation guards against accidental \\`C-x' presses."
  (interactive)
  (when (y-or-n-p "Stop the selected agent? ")
    (claude-code-send-ctrl-x)))

(defun claude-code-vterm-agent-mode-open ()
  "Open the selected agent and exit `claude-code-vterm-agent-mode'.
Sends Return to Claude Code."
  (interactive)
  (claude-code-send-return)
  (claude-code-vterm-agent-mode -1))

(defun claude-code-vterm-agent-mode-open-alt ()
  "Open the selected agent via Alt+1 and exit `claude-code-vterm-agent-mode'."
  (interactive)
  (claude-code-send-meta-1)
  (claude-code-vterm-agent-mode -1))

(defun claude-code-vterm-agent-mode-quit ()
  "Quit the agents view and exit `claude-code-vterm-agent-mode'.
Sends Escape to Claude Code."
  (interactive)
  (claude-code-send-escape)
  (claude-code-vterm-agent-mode -1))

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
    ("h" "Toggle thinking mode (Tab)" claude-code-send-tab)
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
    ("h" "Help (/help)" claude-code-help)
    ("p" "Plan (/plan)" claude-code-plan)]
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

(transient-define-prefix claude-code-agent-view-transient ()
  "Claude Code agents view menu.
Shown with \\`?' in `claude-code-vterm-agent-mode'."
  ["Claude Code Agents"
   ["Select"
    ("n" "Next agent" claude-code-send-down :transient t)
    ("p" "Previous agent" claude-code-send-up :transient t)]
   ["Actions"
    ("r" "Rename agent" claude-code-agent-view-rename)
    ("t" "Pin to top" claude-code-send-ctrl-t)
    ("k" "Stop agent" claude-code-vterm-agent-mode-stop)
    ("v" "Switch view" claude-code-send-ctrl-s)]
   ["Open / Quit"
    ("RET" "Open selected agent" claude-code-vterm-agent-mode-open)
    ("1" "Open (Alt+1)" claude-code-vterm-agent-mode-open-alt)
    ("q" "Quit agents view" claude-code-vterm-agent-mode-quit)]])

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
