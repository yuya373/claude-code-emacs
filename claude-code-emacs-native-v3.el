;;; claude-code-emacs-native-v3.el --- Native UI v3 - Interactive mode with JSON -*- lexical-binding: t; -*-

;;; Commentary:
;; Version 3: Use interactive mode (no -p) with JSON communication
;; This keeps the process alive for continuous conversation

;;; Code:

(require 'json)
(require 'project)
(require 'markdown-mode nil t)

(defgroup claude-code-native-v3 nil
  "Native interface v3 for Claude Code."
  :group 'tools)

(defcustom claude-code-native-v3-claude-command "claude"
  "Path to the claude command."
  :type 'string
  :group 'claude-code-native-v3)

(defcustom claude-code-native-v3-model "claude-3-5-sonnet-20241022"
  "Default model to use."
  :type 'string
  :group 'claude-code-native-v3)

(defcustom claude-code-native-v3-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'claude-code-native-v3)

;;; Session Management

(defvar-local claude-code-native-v3-process nil
  "Current Claude process for this buffer.")

(defvar-local claude-code-native-v3-session-id nil
  "Current session ID.")

(defvar-local claude-code-native-v3-output-buffer ""
  "Buffer for accumulating output.")

(defvar-local claude-code-native-v3-in-json-mode nil
  "Whether we're in JSON output mode.")

;;; Buffer Management

(defun claude-code-native-v3-buffer-name ()
  "Get buffer name for current project."
  (let ((root (project-root (project-current))))
    (format "*claude-native-v3:%s*"
            (file-name-nondirectory (directory-file-name root)))))

(defun claude-code-native-v3-get-or-create-buffer ()
  "Get or create the Claude Code buffer."
  (let* ((buffer-name (claude-code-native-v3-buffer-name))
         (buffer (get-buffer buffer-name)))
    (or buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (claude-code-native-v3-mode)
          (current-buffer)))))

;;; Process Management

(defun claude-code-native-v3-start-process ()
  "Start Claude in interactive mode."
  (let ((process (make-process
                  :name "claude-native-v3"
                  :buffer (current-buffer)
                  :command (list claude-code-native-v3-claude-command
                                 "--model" claude-code-native-v3-model)
                  :filter #'claude-code-native-v3-process-filter
                  :sentinel #'claude-code-native-v3-process-sentinel)))
    (setq claude-code-native-v3-process process)
    (message "Started Claude in interactive mode")))

(defun claude-code-native-v3-send-prompt (prompt)
  "Send PROMPT to Claude Code."
  (interactive "sPrompt: ")
  (let ((buffer (claude-code-native-v3-get-or-create-buffer)))
    (with-current-buffer buffer
      ;; Start process if needed
      (unless (and claude-code-native-v3-process
                   (process-live-p claude-code-native-v3-process))
        (claude-code-native-v3-start-process)
        ;; Wait for welcome message
        (sit-for 1))
      
      ;; Insert user prompt
      (goto-char (point-max))
      (insert (propertize (format "\n## User\n%s\n\n" prompt)
                          'face 'font-lock-keyword-face))
      (insert (propertize "## Assistant\n" 'face 'font-lock-function-name-face))
      
      ;; Send prompt to process
      (process-send-string claude-code-native-v3-process (concat prompt "\n")))
    
    (pop-to-buffer buffer)))

(defun claude-code-native-v3-process-filter (process output)
  "Handle OUTPUT from Claude PROCESS."
  (with-current-buffer (process-buffer process)
    (when claude-code-native-v3-debug
      (message "Raw output: %s" output))
    
    ;; Accumulate output
    (setq claude-code-native-v3-output-buffer 
          (concat claude-code-native-v3-output-buffer output))
    
    ;; Check for JSON mode activation
    (when (string-match "{\"type\":\"" claude-code-native-v3-output-buffer)
      (setq claude-code-native-v3-in-json-mode t))
    
    ;; Process based on mode
    (if claude-code-native-v3-in-json-mode
        (claude-code-native-v3-process-json-output)
      (claude-code-native-v3-process-text-output output))))

(defun claude-code-native-v3-process-json-output ()
  "Process JSON output."
  (let ((lines (split-string claude-code-native-v3-output-buffer "\n")))
    (while (> (length lines) 1)
      (let ((line (car lines)))
        (when (and (> (length line) 0)
                   (string-match-p "^{" line))
          (condition-case nil
              (let ((json-data (json-parse-string line :object-type 'alist)))
                (claude-code-native-v3-handle-json json-data))
            (json-parse-error nil))))
      (setq lines (cdr lines)))
    ;; Keep incomplete line
    (setq claude-code-native-v3-output-buffer (car lines))))

(defun claude-code-native-v3-handle-json (json-data)
  "Handle parsed JSON-DATA."
  (pcase (alist-get 'type json-data)
    ("system"
     (when (equal (alist-get 'subtype json-data) "init")
       (setq claude-code-native-v3-session-id (alist-get 'session_id json-data))))
    
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
     (goto-char (point-max))
     (when (equal (alist-get 'subtype json-data) "success")
       (insert (propertize
                (format "\n[Done: %.2fs, $%.4f]\n"
                        (/ (alist-get 'duration_ms json-data) 1000.0)
                        (alist-get 'total_cost_usd json-data))
                'face 'success))))))

(defun claude-code-native-v3-process-text-output (output)
  "Process normal text OUTPUT."
  ;; Skip ANSI codes and formatting
  (unless (or (string-match-p "\033\\[" output)  ; ANSI escape
              (string-match-p "^╭" output)       ; Box drawing
              (string-match-p "^│" output)
              (string-match-p "^╰" output)
              (string-match-p "^>" output)       ; Prompt
              (string-match-p "^ ⏵" output))     ; Status line
    (goto-char (point-max))
    (insert output)))

(defun claude-code-native-v3-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert (propertize (format "\n[Process %s]\n" (string-trim event)) 'face 'warning))
    (setq claude-code-native-v3-process nil)))

;;; UI Mode

(defvar claude-code-native-v3-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-native-v3-send-prompt)
    (define-key map (kbd "C-c C-k") #'claude-code-native-v3-kill-process)
    map)
  "Keymap for claude-code-native-v3-mode.")

(define-derived-mode claude-code-native-v3-mode markdown-mode "Claude-Native-v3"
  "Major mode for native Claude Code interface v3."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun claude-code-native-v3-kill-process ()
  "Kill the Claude process."
  (interactive)
  (when (and claude-code-native-v3-process
             (process-live-p claude-code-native-v3-process))
    (kill-process claude-code-native-v3-process)))

;;;###autoload
(defun claude-code-native-v3 ()
  "Start native Claude Code interface v3."
  (interactive)
  (unless (executable-find claude-code-native-v3-claude-command)
    (error "Claude command not found!"))
  
  (let ((buffer (claude-code-native-v3-get-or-create-buffer)))
    (switch-to-buffer buffer)
    (when (= (point-max) 1)
      (insert (propertize "# Claude Code Native v3 (Interactive Mode)\n\n" 'face 'info-title-1))
      (insert "This version uses interactive mode without -p flag.\n")
      (insert "The process stays alive between messages.\n\n")
      (insert "Commands:\n")
      (insert "  C-c C-c - Send prompt\n")
      (insert "  C-c C-k - Kill process\n\n"))))

(provide 'claude-code-emacs-native-v3)
;;; claude-code-emacs-native-v3.el ends here