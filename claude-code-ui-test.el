;;; test-claude-code-ui.el --- Tests for UI, transient, and modes -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-ui module

;;; Code:

(require 'ert)
(require 'claude-code-ui)
(require 'cl-lib)

;;; Tests for mode definitions

(ert-deftest test-claude-code-vterm-mode ()
  "Test vterm mode setup."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)

    ;; Test mode inheritance
    (should (derived-mode-p 'vterm-mode))

    ;; Test display settings
    (should (eq display-line-numbers-mode nil))))

(ert-deftest test-claude-code-vterm-scroll-mode-keymap ()
  "Test that scroll minor mode binds the expected keys."
  (let ((map claude-code-vterm-scroll-mode-map))
    (should (eq (lookup-key map (kbd "<prior>"))
                'claude-code-send-page-up))
    (should (eq (lookup-key map (kbd "<next>"))
                'claude-code-send-page-down))
    (should (eq (lookup-key map (kbd "S-<up>"))
                'claude-code-send-line-up))
    (should (eq (lookup-key map (kbd "S-<down>"))
                'claude-code-send-line-down))
    (should (eq (lookup-key map (kbd "C-<end>"))
                'claude-code-send-ctrl-end))
    ;; Toggle-back keys
    (should (eq (lookup-key map (kbd "C-c C-s"))
                'claude-code-vterm-scroll-mode))
    (should (eq (lookup-key map (kbd "q"))
                'claude-code-vterm-scroll-mode))))

(ert-deftest test-claude-code-vterm-mode-toggle-scroll-binding ()
  "Test that vterm mode keymap binds C-c C-s to toggle scroll mode."
  (should (eq (lookup-key claude-code-vterm-mode-map (kbd "C-c C-s"))
              'claude-code-vterm-scroll-mode)))

(ert-deftest test-claude-code-vterm-scroll-mode-non-vterm-buffer ()
  "Test that scroll mode refuses to enable in non-vterm buffers."
  (with-temp-buffer
    (should-error (claude-code-vterm-scroll-mode 1) :type 'user-error)
    (should-not claude-code-vterm-scroll-mode)))

(ert-deftest test-claude-code-vterm-scroll-mode-remaps-modeline ()
  "Test that enabling scroll mode remaps the mode-line face."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (let ((claude-code-vterm-scroll-mode-highlight-modeline t))
      (should-not claude-code--vterm-scroll-mode-face-cookie)
      (claude-code-vterm-scroll-mode 1)
      ;; A face-remap cookie should be set when scroll mode is enabled
      (should claude-code--vterm-scroll-mode-face-cookie)
      ;; Disabling should clear the cookie
      (claude-code-vterm-scroll-mode -1)
      (should-not claude-code--vterm-scroll-mode-face-cookie))))

(ert-deftest test-claude-code-vterm-scroll-mode-respects-highlight-option ()
  "Test that the mode-line is not remapped when the highlight option is nil."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (let ((claude-code-vterm-scroll-mode-highlight-modeline nil))
      (claude-code-vterm-scroll-mode 1)
      ;; No cookie should be set when highlighting is disabled
      (should-not claude-code--vterm-scroll-mode-face-cookie)
      (claude-code-vterm-scroll-mode -1))))

(ert-deftest test-claude-code-vterm-scroll-mode-lighter-propertized ()
  "Test that the scroll mode lighter is a propertized string with the lighter face."
  (should (boundp 'claude-code-vterm-scroll-mode-lighter))
  (should (stringp claude-code-vterm-scroll-mode-lighter))
  ;; The lighter string should carry the lighter face in its properties
  (should (eq (get-text-property 0 'face claude-code-vterm-scroll-mode-lighter)
              'claude-code-vterm-scroll-mode-lighter-face)))

(ert-deftest test-claude-code-vterm-scroll-mode-lighter-in-minor-mode-alist ()
  "Test that the lighter is exposed via `minor-mode-alist'."
  (let ((entry (assq 'claude-code-vterm-scroll-mode minor-mode-alist)))
    (should entry)
    (should (eq (cadr entry) 'claude-code-vterm-scroll-mode-lighter))))

;;; Tests for agent view minor mode

(ert-deftest test-claude-code-vterm-agent-mode-keymap ()
  "Test that agent view minor mode binds only C-c commands plus M-1.
The agents view accepts free text input (e.g. composing a message to an
agent), so every self-inserting key, RET, the arrows, and the TUI's own
control keys must pass through to the terminal untouched."
  (let ((map claude-code-vterm-agent-mode-map))
    ;; Mode commands live on the C-c prefix
    (should (eq (lookup-key map (kbd "C-c C-r"))
                'claude-code-agent-view-rename))
    (should (eq (lookup-key map (kbd "C-c C-x"))
                'claude-code-vterm-agent-mode-stop))
    (should (eq (lookup-key map (kbd "C-c C-a"))
                'claude-code-vterm-agent-mode))
    ;; C-c ? shows the command menu (C-c + punctuation is the
    ;; conventional minor mode territory)
    (should (eq (lookup-key map (kbd "C-c ?"))
                'claude-code-agent-view-transient))
    ;; M-1 cannot pass through vterm (it runs digit-argument), so the
    ;; TUI's Alt+1 open shortcut needs an explicit binding
    (should (eq (lookup-key map (kbd "M-1"))
                'claude-code-vterm-agent-mode-open-alt))
    ;; Everything else must pass through to the terminal
    (dolist (key '("n" "p" "q" "?" "1" "r" "t" "k" "v"
                   "RET" "<return>" "<up>" "<down>" "<escape>"
                   "C-s" "C-r" "C-t"))
      (should-not (lookup-key map (kbd key))))))

(ert-deftest test-claude-code-agent-view-transient-defined ()
  "Test that the agent view transient menu is defined."
  (should (fboundp 'claude-code-agent-view-transient)))

(ert-deftest test-claude-code-vterm-mode-agent-view-binding ()
  "Test that vterm mode keymap binds C-c C-a to enter the agent view."
  (should (eq (lookup-key claude-code-vterm-mode-map (kbd "C-c C-a"))
              'claude-code-vterm-agent-view)))

(ert-deftest test-claude-code-vterm-agent-view-enters-mode ()
  "Test that entering the agent view enables the mode in the Claude buffer
and sends Left."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (let ((keys-sent nil)
          (buf (current-buffer)))
      (cl-letf (((symbol-function 'claude-code-ensure-buffer)
                 (lambda () buf))
                ((symbol-function 'claude-code-send-left)
                 (lambda () (push 'left keys-sent))))
        (claude-code-vterm-agent-view)
        (should (member 'left keys-sent))
        (should claude-code-vterm-agent-mode)
        (claude-code-vterm-agent-mode -1)))))

(ert-deftest test-claude-code-vterm-agent-view-no-session ()
  "Test that entering the agent view without a session signals an error
and does not leave the current buffer stuck in the mode."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (cl-letf (((symbol-function 'claude-code-ensure-buffer)
               (lambda () (error "No Claude Code session for this project"))))
      (should-error (claude-code-vterm-agent-view))
      (should-not claude-code-vterm-agent-mode))))

(ert-deftest test-claude-code-vterm-agent-view-non-vterm-buffer ()
  "Test that the agent view refuses to enable in non-vterm buffers."
  (with-temp-buffer
    (should-error (claude-code-vterm-agent-mode 1) :type 'user-error)
    (should-not claude-code-vterm-agent-mode)))

(ert-deftest test-claude-code-vterm-agent-mode-plain-vterm-buffer ()
  "Test that agent mode refuses to enable in a non-Claude vterm buffer.
All agent mode commands act on the project's Claude Code buffer, so
enabling the mode in a plain vterm shell would silently redirect that
terminal's keys away from it."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (vterm-mode)
    (should-error (claude-code-vterm-agent-mode 1) :type 'user-error)
    (should-not claude-code-vterm-agent-mode)))

(ert-deftest test-claude-code-vterm-agent-mode-stop-confirms ()
  "Test that stopping an agent asks for confirmation first."
  (let ((keys-sent nil))
    (cl-letf (((symbol-function 'claude-code-send-ctrl-x)
               (lambda () (push 'ctrl-x keys-sent))))
      ;; Confirmed: Ctrl+X is sent
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) t)))
        (claude-code-vterm-agent-mode-stop)
        (should (member 'ctrl-x keys-sent)))
      ;; Declined: nothing is sent
      (setq keys-sent nil)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
        (claude-code-vterm-agent-mode-stop)
        (should-not keys-sent)))))

(ert-deftest test-claude-code-vterm-agent-mode-open-exits-mode ()
  "Test that RET sends Return and exits the agent view mode."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (claude-code-vterm-agent-mode 1)
    (let ((keys-sent nil))
      (cl-letf (((symbol-function 'claude-code-send-return)
                 (lambda () (push 'return keys-sent))))
        (claude-code-vterm-agent-mode-open)
        (should (member 'return keys-sent))
        (should-not claude-code-vterm-agent-mode)))))

(ert-deftest test-claude-code-vterm-agent-mode-open-alt-exits-mode ()
  "Test that M-1 sends Alt+1 and exits the agent view mode."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (claude-code-vterm-agent-mode 1)
    (let ((keys-sent nil))
      (cl-letf (((symbol-function 'claude-code-send-meta-1)
                 (lambda () (push 'meta-1 keys-sent))))
        (claude-code-vterm-agent-mode-open-alt)
        (should (member 'meta-1 keys-sent))
        (should-not claude-code-vterm-agent-mode)))))

(ert-deftest test-claude-code-vterm-agent-mode-quit-exits-mode ()
  "Test that ESC sends Escape and exits the agent view mode."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (claude-code-vterm-agent-mode 1)
    (let ((keys-sent nil))
      (cl-letf (((symbol-function 'claude-code-send-escape)
                 (lambda () (push 'escape keys-sent))))
        (claude-code-vterm-agent-mode-quit)
        (should (member 'escape keys-sent))
        (should-not claude-code-vterm-agent-mode)))))

(ert-deftest test-claude-code-vterm-agent-mode-remaps-modeline ()
  "Test that enabling agent view mode remaps the mode-line face."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (let ((claude-code-vterm-agent-mode-highlight-modeline t))
      (should-not claude-code--vterm-agent-mode-face-cookie)
      (claude-code-vterm-agent-mode 1)
      ;; A face-remap cookie should be set when agent view mode is enabled
      (should claude-code--vterm-agent-mode-face-cookie)
      ;; Disabling should clear the cookie
      (claude-code-vterm-agent-mode -1)
      (should-not claude-code--vterm-agent-mode-face-cookie))))

(ert-deftest test-claude-code-vterm-agent-mode-respects-highlight-option ()
  "Test that the mode-line is not remapped when the highlight option is nil."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)
    (let ((claude-code-vterm-agent-mode-highlight-modeline nil))
      (claude-code-vterm-agent-mode 1)
      ;; No cookie should be set when highlighting is disabled
      (should-not claude-code--vterm-agent-mode-face-cookie)
      (claude-code-vterm-agent-mode -1))))

(ert-deftest test-claude-code-vterm-agent-mode-lighter-propertized ()
  "Test that the agent mode lighter is a propertized string with the lighter face."
  (should (boundp 'claude-code-vterm-agent-mode-lighter))
  (should (stringp claude-code-vterm-agent-mode-lighter))
  ;; The lighter string should carry the lighter face in its properties
  (should (eq (get-text-property 0 'face claude-code-vterm-agent-mode-lighter)
              'claude-code-vterm-agent-mode-lighter-face)))

(ert-deftest test-claude-code-vterm-mode-copy-mode-cursor-visibility ()
  "Test that cursor becomes visible when entering vterm-copy-mode.
By default, `cursor-type' is set to nil in `claude-code-vterm-mode' to
reduce flicker since vterm draws its own cursor.  However, vterm stops
drawing its cursor in `vterm-copy-mode', so we need Emacs' built-in
cursor to be shown there."
  (skip-unless (fboundp 'vterm-mode))
  (with-temp-buffer
    (claude-code-vterm-mode)

    ;; Initial state: cursor-type should be nil (flicker prevention)
    (should (eq cursor-type nil))

    ;; Simulate entering vterm-copy-mode: cursor should become visible
    (setq-local vterm-copy-mode t)
    (run-hooks 'vterm-copy-mode-hook)
    (should cursor-type)

    ;; Simulate exiting vterm-copy-mode: cursor-type should return to nil
    (setq-local vterm-copy-mode nil)
    (run-hooks 'vterm-copy-mode-hook)
    (should (eq cursor-type nil))))

;;; Tests for LSP integration

(ert-deftest test-claude-code-lsp-integration ()
  "Test LSP mode integration."
  (skip-unless (featurep 'lsp-mode))
  (let ((lsp-language-id-configuration nil))
    (with-temp-buffer
      (claude-code-prompt-mode)
      (should (member '(claude-code-prompt-mode . "markdown")
                      lsp-language-id-configuration)))))

;;; Tests for vterm multiline buffer filter

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-disabled ()
  "Test vterm multiline buffer filter when feature is disabled."
  (let ((orig-fun-called nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output nil))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer))))
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (setq orig-fun-called t))
           test-process
           "test input")
          (should orig-fun-called))))))

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-simple-input ()
  "Test vterm multiline buffer filter with simple input (no escape sequences)."
  (let ((orig-fun-called nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output t))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'claude-code-buffer-name) (lambda () (buffer-name))))
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (setq orig-fun-called t))
           test-process
           "simple text")
          (should orig-fun-called))))))

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-multiline ()
  "Test vterm multiline buffer filter with escape sequences."
  (let ((orig-fun-called nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output t)
            (claude-code-vterm-multiline-delay 0.001))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'claude-code-buffer-name) (lambda () (buffer-name)))
                  ((symbol-function 'get-buffer-process) (lambda (_) test-process))
                  ((symbol-function 'process-live-p) (lambda (_) t)))
          ;; Input with multiple escape sequences
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (setq orig-fun-called t))
           test-process
           "\033[K\033[1;1H\033[2A\033[3B")
          ;; Should not be called immediately
          (should-not orig-fun-called)
          ;; Should have set up buffer
          (should claude-code--vterm-multiline-buffer)
          ;; Wait for timer
          (sleep-for 0.01)
          ;; Now it should have been called
          (should orig-fun-called))))))

(ert-deftest test-claude-code--vterm-cleanup-multiline-timer ()
  "Test vterm multiline timer cleanup function."
  (with-temp-buffer
    ;; Set up some test state
    (setq-local claude-code--vterm-multiline-buffer "test buffer content")
    (setq-local claude-code--vterm-multiline-buffer-timer
                (run-at-time 10 nil (lambda () nil))) ; Timer that won't fire

    ;; Call cleanup
    (claude-code--vterm-cleanup-multiline-timer)

    ;; Check that everything is cleaned up
    (should-not claude-code--vterm-multiline-buffer)
    (should-not claude-code--vterm-multiline-buffer-timer)))

(ert-deftest test-claude-code--vterm-multiline-error-handling ()
  "Test error handling in vterm multiline buffer filter during delayed processing."
  (let ((error-message nil)
        (test-process 'mock-process))
    (with-temp-buffer
      (let ((claude-code-vterm-buffer-multiline-output t)
            (claude-code-vterm-multiline-delay 0.001))
        (cl-letf (((symbol-function 'process-buffer) (lambda (_) (current-buffer)))
                  ((symbol-function 'claude-code-buffer-name) (lambda () (buffer-name)))
                  ((symbol-function 'get-buffer-process) (lambda (_) test-process))
                  ((symbol-function 'process-live-p) (lambda (_) t))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (setq error-message (apply #'format fmt args)))))
          ;; Process multiline input with error-throwing function
          (claude-code--vterm-multiline-buffer-filter
           (lambda (proc input)
             (error "Test error"))
           test-process
           "\033[K\033[1;1H\033[2A\033[3B") ; Multiple escape sequences to trigger buffering
          ;; Wait for timer to process
          (sleep-for 0.01)
          ;; Should have captured error message
          (should error-message)
          (should (string-match "Error in vterm filter:" error-message))
          (should (string-match "Test error" error-message)))))))

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-ambient-buffer-not-in-project ()
  "Test filter resolves the buffer name against the process buffer.
Regression test for issue #17: a process filter runs with whatever buffer
happened to be current when output arrived.  When that ambient buffer is
outside a projectile project, resolving `claude-code-buffer-name' against
it signals a `user-error', flooding *Messages*.  The filter must resolve
the name relative to the process buffer instead."
  (let ((orig-fun-called nil)
        (proc-buffer (get-buffer-create "*claude:/project*"))
        (ambient-buffer (get-buffer-create "*not-a-project*"))
        (test-process 'mock-process))
    (unwind-protect
        (let ((claude-code-vterm-buffer-multiline-output t))
          (cl-letf (((symbol-function 'process-buffer) (lambda (_) proc-buffer))
                    ;; Simulate projectile: only the process buffer's
                    ;; default-directory resolves to a project; the ambient
                    ;; buffer signals via `claude-code-normalize-project-root'.
                    ((symbol-function 'claude-code-buffer-name)
                     (lambda ()
                       (if (eq (current-buffer) proc-buffer)
                           "*claude:/project*"
                         (user-error "Current directory is not part of a project")))))
            ;; Run the filter from the ambient (non-project) buffer.
            (with-current-buffer ambient-buffer
              (claude-code--vterm-multiline-buffer-filter
               (lambda (_proc _input)
                 (setq orig-fun-called t))
               test-process
               "simple text"))
            ;; The filter must not have signaled, and should have passed
            ;; the input through since this is the Claude process buffer.
            (should orig-fun-called)))
      (kill-buffer proc-buffer)
      (kill-buffer ambient-buffer))))

(ert-deftest test-claude-code--vterm-multiline-buffer-filter-dead-process-buffer ()
  "Test filter passes input through when the process buffer is dead.
When the process buffer has been killed, both the resolved Claude buffer
name and `buffer-name' of the dead buffer are nil, so a naive `equal'
comparison matches and the filter enters the buffering branch, where
`with-current-buffer' on the dead buffer signals once per output chunk.
The filter must instead pass the input through to ORIG-FUN untouched."
  (let ((orig-fun-called nil)
        (proc-buffer (get-buffer-create "*claude:/project*"))
        (test-process 'mock-process))
    (kill-buffer proc-buffer)
    (let ((claude-code-vterm-buffer-multiline-output t))
      (cl-letf (((symbol-function 'process-buffer) (lambda (_) proc-buffer)))
        (claude-code--vterm-multiline-buffer-filter
         (lambda (_proc _input)
           (setq orig-fun-called t))
         test-process
         "simple text")
        ;; The filter must not have signaled and must have passed through.
        (should orig-fun-called)))))

;;; Tests for transient menus

(ert-deftest test-claude-code-transient-defined ()
  "Test that transient menus are properly defined."
  (should (fboundp 'claude-code-transient))
  (should (fboundp 'claude-code-prompt-transient)))

(provide 'test-claude-code-ui)
;;; test-claude-code-ui.el ends here
