;;; claude-code-emacs-native-v2.el --- Native UI with streaming JSON input -*- lexical-binding: t; -*-

;;; Commentary:
;; Version 2 of the native interface using streaming JSON input
;; This allows continuous conversation without restarting the process

;;; Code:

(require 'json)
(require 'project)
(require 'markdown-mode nil t)

(defgroup claude-code-native-v2 nil
  "Native interface v2 for Claude Code."
  :group 'tools)

(defcustom claude-code-native-v2-claude-command "claude"
  "Path to the claude command."
  :type 'string
  :group 'claude-code-native-v2)

(defcustom claude-code-native-v2-model "claude-3-5-sonnet-20241022"
  "Default model to use."
  :type 'string
  :group 'claude-code-native-v2)

(defcustom claude-code-native-v2-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'claude-code-native-v2)

;;; Session Management

(defvar claude-code-native-v2-sessions (make-hash-table :test 'equal)
  "Hash table mapping project roots to session info.")

(defvar-local claude-code-native-v2-session-id nil
  "Current session ID for this buffer.")

(defvar-local claude-code-native-v2-process nil
  "Current Claude process for this buffer.")

(defvar-local claude-code-native-v2-partial-json ""
  "Partial JSON data from streaming responses.")

;;; Buffer Management

(defun claude-code-native-v2-buffer-name (&optional project-root)
  "Get buffer name for PROJECT-ROOT."
  (let ((root (or project-root (project-root (project-current)))))
    (format "*claude-native-v2:%s*"
            (file-name-nondirectory (directory-file-name root)))))

(defun claude-code-native-v2-get-or-create-buffer ()
  "Get or create the Claude Code buffer for current project."
  (let* ((buffer-name (claude-code-native-v2-buffer-name))
         (buffer (get-buffer buffer-name)))
    (or buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (claude-code-native-v2-mode)
          (current-buffer)))))

;;; Process Management

(defun claude-code-native-v2-ensure-process (initial-prompt)
  "Ensure a Claude process is running with INITIAL-PROMPT."
  (unless (and claude-code-native-v2-process
               (process-live-p claude-code-native-v2-process))
    (let* ((project-root (project-root (project-current)))
           (session-id (gethash project-root claude-code-native-v2-sessions))
           (command-args (append
                          (list claude-code-native-v2-claude-command)
                          (list "-p"
                                "--output-format" "stream-json"
                                "--input-format" "stream-json"
                                "--verbose"
                                ))))
      (setq claude-code-native-v2-process
            (make-process
             :name (format "claude-native-v2-%s" (or session-id "new"))
             :buffer (current-buffer)
             :command command-args
             :filter #'claude-code-native-v2-process-filter
             :sentinel #'claude-code-native-v2-process-sentinel))
      ;; Send initial prompt immediately
      (let ((json-message (json-encode
                           `((type . "user")
                             (message . ((role . "user")
                                         (content . [((type . "text")
                                                      (text . ,initial-prompt))])))))))
        (process-send-string claude-code-native-v2-process (concat json-message "\n")))
      (message "Started new Claude process with initial prompt"))))

(defun claude-code-native-v2-send-prompt (prompt)
  "Send PROMPT to Claude Code."
  (interactive "sPrompt: ")
  (let* ((buffer (claude-code-native-v2-get-or-create-buffer))
         (project-root (project-root (project-current))))
    (with-current-buffer buffer
      ;; Insert user prompt in buffer
      (goto-char (point-max))
      (insert (propertize (format "\n## User\n%s\n\n" prompt)
                          'face 'font-lock-keyword-face))

      ;; Check if process exists
      (if (and claude-code-native-v2-process
               (process-live-p claude-code-native-v2-process))
          ;; Process exists, just send message
          (let ((json-message (json-encode
                               `((type . "user")
                                 (message . ((role . "user")
                                             (content . [((type . "text")
                                                          (text . ,prompt))])))))))
            (when claude-code-native-v2-debug
              (message "Sending to existing process: %s" json-message))
            (process-send-string claude-code-native-v2-process (concat json-message "\n")))
        ;; No process, create with initial prompt
        (claude-code-native-v2-ensure-process prompt))

      (insert (propertize "## Assistant\n" 'face 'font-lock-function-name-face)))

    ;; Show the buffer
    (pop-to-buffer buffer)))

(defun claude-code-native-v2-process-filter (process output)
  "Handle OUTPUT from Claude PROCESS."
  (with-current-buffer (process-buffer process)
    ;; Debug output
    (when claude-code-native-v2-debug
      (goto-char (point-max))
      (insert (propertize (format "\n[DEBUG] Raw output: %s\n" output)
                          'face 'font-lock-comment-face)))

    ;; Process JSON lines
    (let ((lines (split-string (concat claude-code-native-v2-partial-json output) "\n")))
      (while (> (length lines) 1)
        (let ((line (car lines)))
          (when (> (length line) 0)
            (claude-code-native-v2-handle-json-line line)))
        (setq lines (cdr lines)))
      (setq claude-code-native-v2-partial-json (car lines)))))

(defun claude-code-native-v2-handle-json-line (line)
  "Process a complete JSON LINE from Claude."
  (condition-case err
      (when (string-match-p "^{" line)
        (let ((json-data (json-parse-string line :object-type 'alist)))
          (pcase (alist-get 'type json-data)
            ;; System initialization
            ("system"
             (when (equal (alist-get 'subtype json-data) "init")
               (let ((session-id (alist-get 'session_id json-data))
                     (project-root (project-root (project-current))))
                 (setq claude-code-native-v2-session-id session-id)
                 (puthash project-root session-id claude-code-native-v2-sessions))

               (goto-char (point-max))
               (insert (propertize "=== Session Info ===\n" 'face 'font-lock-comment-face))
               (insert (format "Session ID: %s\n" (alist-get 'session_id json-data)))
               (insert (format "Model: %s\n" (alist-get 'model json-data)))
               (insert (propertize "==================\n\n" 'face 'font-lock-comment-face))))

            ;; Assistant response
            ("assistant"
             (let ((message (alist-get 'message json-data)))
               (let ((contents (alist-get 'content message)))
                 (dolist (content (if (vectorp contents)
                                      (append contents nil)
                                    contents))
                   (pcase (alist-get 'type content)
                     ("text"
                      (goto-char (point-max))
                      (insert (alist-get 'text content)))
                     ("tool_use"
                      (goto-char (point-max))
                      (insert (propertize
                               (format "\n🔧 Tool: %s\n" (alist-get 'name content))
                               'face 'font-lock-warning-face))))))))

            ;; Result
            ("result"
             (goto-char (point-max))
             (let ((subtype (alist-get 'subtype json-data)))
               (when (equal subtype "success")
                 (insert (propertize
                          (format "\n✅ Done (%.2fs, $%.4f)\n"
                                  (/ (alist-get 'duration_ms json-data) 1000.0)
                                  (alist-get 'total_cost_usd json-data))
                          'face 'success))))))))
    (json-parse-error
     (when claude-code-native-v2-debug
       (message "JSON parse error: %s" err)))))

(defun claude-code-native-v2-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (cond
     ((string-match-p "finished" event)
      (insert (propertize "\n[Process ended]\n" 'face 'warning))
      (setq claude-code-native-v2-process nil))
     ((string-match-p "exited abnormally" event)
      (insert (propertize (format "\n[Process error: %s]\n" event) 'face 'error))
      (setq claude-code-native-v2-process nil)))))

;;; UI Mode

(defvar claude-code-native-v2-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-native-v2-send-region-or-prompt)
    (define-key map (kbd "C-c C-p") #'claude-code-native-v2-send-prompt)
    (define-key map (kbd "C-c C-k") #'claude-code-native-v2-kill-process)
    (define-key map (kbd "C-c C-n") #'claude-code-native-v2-new-session)
    map)
  "Keymap for claude-code-native-v2-mode.")

(define-derived-mode claude-code-native-v2-mode markdown-mode "Claude-Native-v2"
  "Major mode for native Claude Code interface v2."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (read-only-mode -1))

;;; Interactive Commands

(defun claude-code-native-v2-send-region-or-prompt (start end)
  "Send region from START to END, or prompt interactively."
  (interactive "r")
  (if (use-region-p)
      (claude-code-native-v2-send-prompt (buffer-substring-no-properties start end))
    (call-interactively #'claude-code-native-v2-send-prompt)))

(defun claude-code-native-v2-kill-process ()
  "Kill the current Claude process."
  (interactive)
  (when (and claude-code-native-v2-process
             (process-live-p claude-code-native-v2-process))
    (kill-process claude-code-native-v2-process)
    (message "Claude process killed")))

(defun claude-code-native-v2-new-session ()
  "Start a new session."
  (interactive)
  (claude-code-native-v2-kill-process)
  (let ((project-root (project-root (project-current))))
    (remhash project-root claude-code-native-v2-sessions)
    (setq claude-code-native-v2-session-id nil))
  (message "Ready for new session"))

;;;###autoload
(defun claude-code-native-v2 ()
  "Start native Claude Code interface v2."
  (interactive)
  (unless (executable-find claude-code-native-v2-claude-command)
    (error "Claude command not found!"))

  (let ((buffer (claude-code-native-v2-get-or-create-buffer)))
    (switch-to-buffer buffer)
    (when (= (point-max) 1)
      (insert (propertize "# Claude Code Native v2 (Streaming Input)\n\n" 'face 'info-title-1))
      (insert "This version keeps the process running and sends messages via stdin.\n\n")
      (insert "Commands:\n")
      (insert "  C-c C-c - Send region or prompt\n")
      (insert "  C-c C-p - Send prompt\n")
      (insert "  C-c C-k - Kill process\n")
      (insert "  C-c C-n - New session\n\n"))))

(provide 'claude-code-emacs-native-v2)
;;; claude-code-emacs-native-v2.el ends here
