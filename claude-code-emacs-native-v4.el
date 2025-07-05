;;; claude-code-emacs-native-v4.el --- Native UI v4 - Shell command approach -*- lexical-binding: t; -*-

;;; Commentary:
;; Version 4: Use start-process-shell-command with pipe for initial message

;;; Code:

(require 'json)
(require 'project)
(require 'markdown-mode nil t)

(defgroup claude-code-native-v4 nil
  "Native interface v4 for Claude Code."
  :group 'tools)

(defcustom claude-code-native-v4-claude-command "claude"
  "Path to the claude command."
  :type 'string
  :group 'claude-code-native-v4)

(defcustom claude-code-native-v4-model "claude-3-5-sonnet-20241022"
  "Default model to use."
  :type 'string
  :group 'claude-code-native-v4)

(defcustom claude-code-native-v4-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'claude-code-native-v4)

;;; Buffer Management

(defun claude-code-native-v4-buffer-name ()
  "Get buffer name for current project."
  (let ((root (project-root (project-current))))
    (format "*claude-native-v4:%s*"
            (file-name-nondirectory (directory-file-name root)))))

(defun claude-code-native-v4-get-or-create-buffer ()
  "Get or create the Claude Code buffer."
  (let* ((buffer-name (claude-code-native-v4-buffer-name))
         (buffer (get-buffer buffer-name)))
    (or buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (claude-code-native-v4-mode)
          (current-buffer)))))

;;; Process Management

(defvar-local claude-code-native-v4-partial-json ""
  "Partial JSON data from streaming responses.")

(defvar-local claude-code-native-v4-session-id nil
  "Current session ID.")

(defun claude-code-native-v4-send-prompt (prompt)
  "Send PROMPT to Claude Code."
  (interactive "sPrompt: ")
  (let* ((buffer (claude-code-native-v4-get-or-create-buffer))
         (json-message (json-encode
                        `((type . "user")
                          (message . ((role . "user")
                                      (content . [((type . "text")
                                                   (text . ,prompt))]))))))
         (shell-command (format "echo '%s' | %s -p --output-format=stream-json --input-format=stream-json --verbose --model=%s"
                                (replace-regexp-in-string "'" "'\\''" json-message)
                                claude-code-native-v4-claude-command
                                claude-code-native-v4-model)))
    (with-current-buffer buffer
      ;; Insert user prompt
      (goto-char (point-max))
      (insert (propertize (format "\n## User\n%s\n\n" prompt)
                          'face 'font-lock-keyword-face))
      (insert (propertize "## Assistant\n" 'face 'font-lock-function-name-face))
      
      ;; Start process with shell command
      (let ((process (start-process-shell-command
                      "claude-native-v4"
                      buffer
                      shell-command)))
        (set-process-filter process #'claude-code-native-v4-process-filter)
        (set-process-sentinel process #'claude-code-native-v4-process-sentinel)))
    
    (pop-to-buffer buffer)))

(defun claude-code-native-v4-process-filter (process output)
  "Handle OUTPUT from Claude PROCESS."
  (with-current-buffer (process-buffer process)
    (when claude-code-native-v4-debug
      (goto-char (point-max))
      (insert (propertize (format "\n[DEBUG] %s\n" output)
                          'face 'font-lock-comment-face)))
    
    ;; Process JSON lines
    (let ((lines (split-string (concat claude-code-native-v4-partial-json output) "\n")))
      (while (> (length lines) 1)
        (let ((line (car lines)))
          (when (> (length line) 0)
            (claude-code-native-v4-handle-json-line line)))
        (setq lines (cdr lines)))
      (setq claude-code-native-v4-partial-json (car lines)))))

(defun claude-code-native-v4-handle-json-line (line)
  "Process a complete JSON LINE."
  (condition-case nil
      (when (string-match-p "^{" line)
        (let ((json-data (json-parse-string line :object-type 'alist)))
          (pcase (alist-get 'type json-data)
            ("system"
             (when (equal (alist-get 'subtype json-data) "init")
               (setq claude-code-native-v4-session-id (alist-get 'session_id json-data))
               (goto-char (point-max))
               (insert (propertize (format "[Session: %s]\n" claude-code-native-v4-session-id)
                                   'face 'font-lock-comment-face))))
            
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
            
            ("result"
             (when (equal (alist-get 'subtype json-data) "success")
               (goto-char (point-max))
               (insert (propertize
                        (format "\n✅ Done (%.2fs, $%.4f)\n"
                                (/ (alist-get 'duration_ms json-data) 1000.0)
                                (alist-get 'total_cost_usd json-data))
                        'face 'success)))))))
    (json-parse-error nil)))

(defun claude-code-native-v4-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (unless (string-match-p "finished" event)
      (insert (propertize (format "[Process: %s]\n" (string-trim event)) 'face 'error)))))

;;; UI Mode

(defvar claude-code-native-v4-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-native-v4-send-prompt)
    map)
  "Keymap for claude-code-native-v4-mode.")

(define-derived-mode claude-code-native-v4-mode markdown-mode "Claude-Native-v4"
  "Major mode for native Claude Code interface v4.")

;;;###autoload
(defun claude-code-native-v4 ()
  "Start native Claude Code interface v4."
  (interactive)
  (unless (executable-find claude-code-native-v4-claude-command)
    (error "Claude command not found!"))
  
  (let ((buffer (claude-code-native-v4-get-or-create-buffer)))
    (switch-to-buffer buffer)
    (when (= (point-max) 1)
      (insert (propertize "# Claude Code Native v4 (Shell Command)\n\n" 'face 'info-title-1))
      (insert "Using echo | claude via shell command.\n\n"))))

(provide 'claude-code-emacs-native-v4)
;;; claude-code-emacs-native-v4.el ends here