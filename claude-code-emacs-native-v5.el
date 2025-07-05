;;; claude-code-emacs-native-v5.el --- Native UI v5 - Persistent shell process -*- lexical-binding: t; -*-

;;; Commentary:
;; Version 5: Use shell with persistent stdin for streaming JSON

;;; Code:

(require 'json)
(require 'project)
(require 'markdown-mode nil t)

(defgroup claude-code-native-v5 nil
  "Native interface v5 for Claude Code."
  :group 'tools)

(defcustom claude-code-native-v5-claude-command "claude"
  "Path to the claude command."
  :type 'string
  :group 'claude-code-native-v5)

(defcustom claude-code-native-v5-model "claude-3-5-sonnet-20241022"
  "Default model to use."
  :type 'string
  :group 'claude-code-native-v5)

(defcustom claude-code-native-v5-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'claude-code-native-v5)

;;; Session Management

(defvar-local claude-code-native-v5-process nil
  "Current Claude process.")

(defvar-local claude-code-native-v5-session-id nil
  "Current session ID.")

(defvar-local claude-code-native-v5-partial-json ""
  "Partial JSON data.")

(defvar-local claude-code-native-v5-first-message t
  "Whether this is the first message.")

;;; Buffer Management

(defun claude-code-native-v5-buffer-name ()
  "Get buffer name for current project."
  (let ((root (project-root (project-current))))
    (format "*claude-native-v5:%s*"
            (file-name-nondirectory (directory-file-name root)))))

(defun claude-code-native-v5-get-or-create-buffer ()
  "Get or create the Claude Code buffer."
  (let* ((buffer-name (claude-code-native-v5-buffer-name))
         (buffer (get-buffer buffer-name)))
    (or buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (claude-code-native-v5-mode)
          (current-buffer)))))

;;; Process Management

(defun claude-code-native-v5-ensure-process ()
  "Ensure Claude process is running."
  (unless (and claude-code-native-v5-process
               (process-live-p claude-code-native-v5-process))
    ;; Use shell to keep stdin open
    (let* ((cmd (format "%s -p --output-format=stream-json --input-format=stream-json --verbose --model=%s"
                        claude-code-native-v5-claude-command
                        claude-code-native-v5-model))
           (process (make-process
                     :name "claude-native-v5"
                     :buffer (current-buffer)
                     :command (list shell-file-name shell-command-switch cmd)
                     :connection-type 'pipe  ; Important: use pipe for stdin
                     :filter #'claude-code-native-v5-process-filter
                     :sentinel #'claude-code-native-v5-process-sentinel)))
      (setq claude-code-native-v5-process process)
      (setq claude-code-native-v5-first-message t)
      (message "Started Claude process with streaming JSON"))))

(defun claude-code-native-v5-send-prompt (prompt)
  "Send PROMPT to Claude Code."
  (interactive "sPrompt: ")
  (let ((buffer (claude-code-native-v5-get-or-create-buffer)))
    (with-current-buffer buffer
      ;; Ensure process
      (claude-code-native-v5-ensure-process)

      ;; Insert user prompt
      (goto-char (point-max))
      (insert (propertize (format "\n## User\n%s\n\n" prompt)
                          'face 'font-lock-keyword-face))
      (insert (propertize "## Assistant\n" 'face 'font-lock-function-name-face))

      ;; Send JSON message
      (let ((json-message (json-encode
                           `((type . "user")
                             (message . ((role . "user")
                                         (content . [((type . "text")
                                                      (text . ,prompt))])))))))
        (when claude-code-native-v5-debug
          (message "Sending: %s" json-message))

        ;; For first message, we need to provide input immediately
        (if claude-code-native-v5-first-message
            (progn
              (process-send-string claude-code-native-v5-process
                                   (concat json-message "\n"))
              (setq claude-code-native-v5-first-message nil))
          ;; Subsequent messages
          (process-send-string claude-code-native-v5-process
                               (concat json-message "\n")))))

    (pop-to-buffer buffer)))

(defun claude-code-native-v5-process-filter (process output)
  "Handle OUTPUT from Claude PROCESS."
  (with-current-buffer (process-buffer process)
    (when claude-code-native-v5-debug
      (goto-char (point-max))
      (insert (propertize (format "\n[DEBUG] %s\n" output)
                          'face 'font-lock-comment-face)))

    ;; Check if process ended due to missing input
    (when (string-match-p "Input must be provided" output)
      (goto-char (point-max))
      (insert (propertize "\n[Error: Process needs input on startup]\n" 'face 'error))
      (setq claude-code-native-v5-process nil)
      (return))

    ;; Process JSON lines
    (let ((lines (split-string (concat claude-code-native-v5-partial-json output) "\n")))
      (while (> (length lines) 1)
        (let ((line (car lines)))
          (when (> (length line) 0)
            (claude-code-native-v5-handle-json-line line)))
        (setq lines (cdr lines)))
      (setq claude-code-native-v5-partial-json (car lines)))))

(defun claude-code-native-v5-handle-json-line (line)
  "Process a complete JSON LINE."
  (condition-case nil
      (when (string-match-p "^{" line)
        (let ((json-data (json-parse-string line :object-type 'alist)))
          (pcase (alist-get 'type json-data)
            ("system"
             (when (equal (alist-get 'subtype json-data) "init")
               (setq claude-code-native-v5-session-id (alist-get 'session_id json-data))))

            ("assistant"
             (let ((message (alist-get 'message json-data)))
               (let ((contents (alist-get 'content message)))
                 (dolist (content (if (vectorp contents)
                                      (append contents nil)
                                    contents))
                   (when (equal (alist-get 'type content) "text")
                     (goto-char (point-max))
                     (insert (alist-get 'text content)))))))

            ("result"
             (when (equal (alist-get 'subtype json-data) "success")
               (goto-char (point-max))
               (insert (propertize
                        (format "\n✅ Done (%.2fs, $%.4f)\n"
                                (/ (alist-get 'duration_ms json-data) 1000.0)
                                (alist-get 'total_cost_usd json-data))
                        'face 'success))
               ;; Process should still be alive for next message!
               (message "Ready for next message"))))))
    (json-parse-error nil)))

(defun claude-code-native-v5-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (propertize (format "\n[Process %s]\n" (string-trim event)) 'face 'warning))
    (setq claude-code-native-v5-process nil)))

(defun claude-code-native-v5-kill-process ()
  "Kill the Claude process."
  (interactive)
  (when (and claude-code-native-v5-process
             (process-live-p claude-code-native-v5-process))
    (kill-process claude-code-native-v5-process)))

;;; UI Mode

(defvar claude-code-native-v5-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-native-v5-send-prompt)
    (define-key map (kbd "C-c C-k") #'claude-code-native-v5-kill-process)
    map)
  "Keymap for claude-code-native-v5-mode.")

(define-derived-mode claude-code-native-v5-mode markdown-mode "Claude-Native-v5"
  "Major mode for native Claude Code interface v5.")

;;;###autoload
(defun claude-code-native-v5 ()
  "Start native Claude Code interface v5."
  (interactive)
  (unless (executable-find claude-code-native-v5-claude-command)
    (error "Claude command not found!"))

  (let ((buffer (claude-code-native-v5-get-or-create-buffer)))
    (switch-to-buffer buffer)
    (when (= (point-max) 1)
      (insert (propertize "# Claude Code Native v5 (Persistent Streaming)\n\n" 'face 'info-title-1))
      (insert "Attempting to keep process alive between messages.\n\n"))))

(provide 'claude-code-emacs-native-v5)
;;; claude-code-emacs-native-v5.el ends here
