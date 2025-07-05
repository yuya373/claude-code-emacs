;;; claude-code-emacs-native.el --- Native Emacs UI for Claude Code -*- lexical-binding: t; -*-

;;; Commentary:
;; Prototype for a native Emacs interface to Claude Code using the SDK
;; This replaces the vterm-based approach with a pure Emacs implementation

;;; Code:

(require 'json)
(require 'project)
(require 'markdown-mode nil t)

(defgroup claude-code-native nil
  "Native interface for Claude Code."
  :group 'tools)

(defcustom claude-code-native-claude-command "claude"
  "Path to the claude command."
  :type 'string
  :group 'claude-code-native)

(defcustom claude-code-native-model "claude-3-5-sonnet-20241022"
  "Default model to use."
  :type 'string
  :group 'claude-code-native)

(defcustom claude-code-native-debug nil
  "Enable debug output."
  :type 'boolean
  :group 'claude-code-native)

;;; Session Management

(defvar claude-code-native-sessions (make-hash-table :test 'equal)
  "Hash table mapping project roots to session IDs.")

(defvar-local claude-code-native-session-id nil
  "Current session ID for this buffer.")

(defun claude-code-native-get-session-id (&optional project-root)
  "Get session ID for PROJECT-ROOT."
  (let ((root (or project-root (project-root (project-current)))))
    (gethash root claude-code-native-sessions)))

;;; Buffer Management

(defun claude-code-native-buffer-name (&optional project-root)
  "Get buffer name for PROJECT-ROOT."
  (let ((root (or project-root (project-root (project-current)))))
    (format "*claude-native:%s*"
            (file-name-nondirectory (directory-file-name root)))))

(defun claude-code-native-get-or-create-buffer ()
  "Get or create the Claude Code buffer for current project."
  (let* ((buffer-name (claude-code-native-buffer-name))
         (buffer (get-buffer buffer-name)))
    (or buffer
        (with-current-buffer (get-buffer-create buffer-name)
          (claude-code-native-mode)
          (current-buffer)))))

;;; Process Management

(defvar-local claude-code-native-process nil
  "Current Claude process for this buffer.")

(defvar-local claude-code-native-partial-json ""
  "Partial JSON data from streaming responses.")

(defvar-local claude-code-native-current-response nil
  "Current response being accumulated.")

(defun claude-code-native-send-prompt (prompt)
  "Send PROMPT to Claude Code."
  (interactive "sPrompt: ")
  (let* ((buffer (claude-code-native-get-or-create-buffer))
         (session-id (claude-code-native-get-session-id))
         (project-root (project-root (project-current))))
    (with-current-buffer buffer
      ;; Kill any existing process
      (when (and claude-code-native-process
                 (process-live-p claude-code-native-process))
        (kill-process claude-code-native-process))

      ;; Insert user prompt
      (goto-char (point-max))
      (insert (propertize (format "\n## User\n%s\n\n" prompt)
                          'face 'font-lock-keyword-face))

      ;; Start Claude process
      (let* ((default-directory project-root)
             (command-args (append
                            (list claude-code-native-claude-command)
                            ;; Only add --resume if we have a session ID
                            (when session-id
                              (list "--resume" session-id))
                            (list "-p" prompt
                                  "--output-format" "stream-json"
                                  "--verbose"  ;; Required for stream-json
                                  "--model" claude-code-native-model)))
             (process (make-process
                       :name (format "claude-native-%s" (or session-id "new"))
                       :buffer buffer
                       :command command-args
                       :filter #'claude-code-native-process-filter
                       :sentinel #'claude-code-native-process-sentinel)))
        (setq claude-code-native-process process)
        (setq claude-code-native-current-response nil)
        (insert (propertize "## Assistant\n" 'face 'font-lock-function-name-face))))

    ;; Show the buffer
    (pop-to-buffer buffer)))

(defun claude-code-native-process-filter (process output)
  "Handle OUTPUT from Claude PROCESS."
  (with-current-buffer (process-buffer process)
    ;; Debug: Show raw output
    (when claude-code-native-debug
      (goto-char (point-max))
      (insert (propertize (format "\n[DEBUG] Raw output: %s\n" output)
                          'face 'font-lock-comment-face)))

    (let ((lines (split-string (concat claude-code-native-partial-json output) "\n")))
      ;; Process complete lines
      (while (> (length lines) 1)
        (let ((line (car lines)))
          (when (> (length line) 0)
            (claude-code-native-handle-json-line line)))
        (setq lines (cdr lines)))
      ;; Save partial line
      (setq claude-code-native-partial-json (car lines)))))

(defun claude-code-native-handle-json-line (line)
  "Process a complete JSON LINE from Claude."
  (condition-case err
      (when (string-match-p "^{" line)  ; Only parse lines that start with {
        (let ((json-data (json-parse-string line :object-type 'alist)))
          (pcase (alist-get 'type json-data)
          ;; System message - initialization info
          ("system"
           (when (equal (alist-get 'subtype json-data) "init")
             ;; Store session ID from init message
             (let ((session-id (alist-get 'session_id json-data))
                   (project-root (project-root (project-current))))
               (setq claude-code-native-session-id session-id)
               (puthash project-root session-id claude-code-native-sessions))

             (goto-char (point-max))
             (insert (propertize "=== Session Info ===\n" 'face 'font-lock-comment-face))
             (insert (format "Session ID: %s\n" (alist-get 'session_id json-data)))
             (insert (format "Model: %s\n" (alist-get 'model json-data)))
             (insert (format "Working directory: %s\n" (alist-get 'cwd json-data)))
             (let ((tools (alist-get 'tools json-data)))
               (insert (format "Tools: %s\n" 
                               (if (vectorp tools)
                                   (mapconcat #'identity (append tools nil) ", ")
                                 (mapconcat #'identity tools ", ")))))
             (when-let ((mcp-servers (alist-get 'mcp_servers json-data)))
               (insert "MCP Servers:\n")
               (dolist (server (if (vectorp mcp-servers)
                                   (append mcp-servers nil)  ; Convert vector to list
                                 mcp-servers))
                 (insert (format "  - %s (%s)\n"
                                 (alist-get 'name server)
                                 (alist-get 'status server)))))
             (insert (propertize "==================\n\n" 'face 'font-lock-comment-face))))

          ;; Assistant message with streaming content
          ("assistant"
           (let ((message (alist-get 'message json-data)))
             ;; Handle the message content
             (let ((contents (alist-get 'content message)))
               (dolist (content (if (vectorp contents)
                                    (append contents nil)  ; Convert vector to list
                                  contents))
                 (pcase (alist-get 'type content)
                   ("text"
                    (let ((text (alist-get 'text content)))
                      (goto-char (point-max))
                      (insert text)))
                   ("tool_use"
                    (goto-char (point-max))
                    (insert (propertize
                             (format "\n🔧 Tool: %s\n" (alist-get 'name content))
                             'face 'font-lock-warning-face))
                    (insert (propertize
                             (format "Input: %s\n"
                                     (json-encode (alist-get 'input content)))
                             'face 'font-lock-comment-face))))))))

          ;; User message (shouldn't appear in output but handle it)
          ("user"
           (let ((message (alist-get 'message json-data)))
             (goto-char (point-max))
             (insert (propertize "\n## User\n" 'face 'font-lock-keyword-face))
             (let ((contents (alist-get 'content message)))
               (dolist (content (if (vectorp contents)
                                    (append contents nil)  ; Convert vector to list
                                  contents))
                 (when (equal (alist-get 'type content) "text")
                   (insert (alist-get 'text content)))))
             (insert "\n\n")))

          ;; Result message - completion status
          ("result"
           (goto-char (point-max))
           (let ((subtype (alist-get 'subtype json-data)))
             (cond
              ((equal subtype "success")
               (insert (propertize
                        (format "\n✅ Completed successfully in %.2fs (API: %.2fs)\n"
                                (/ (alist-get 'duration_ms json-data) 1000.0)
                                (/ (alist-get 'duration_api_ms json-data) 1000.0))
                        'face 'success))
               (insert (format "Total turns: %d | Cost: $%.4f\n"
                               (alist-get 'num_turns json-data)
                               (alist-get 'total_cost_usd json-data))))
              ((equal subtype "error_max_turns")
               (insert (propertize
                        "\n⚠️ Reached maximum turns limit\n"
                        'face 'warning)))
              ((equal subtype "error_during_execution")
               (insert (propertize
                        "\n❌ Error during execution\n"
                        'face 'error)))))))))
    (json-parse-error
     (when claude-code-native-debug
       (message "JSON parse error on line: %s (error: %s)" line err)))))

(defun claude-code-native-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (cond
     ((string-match-p "finished" event)
      (insert (propertize "[Process completed]\n" 'face 'success)))
     ((string-match-p "exited abnormally" event)
      (insert (propertize (format "[Process error: %s]\n" event) 'face 'error))
      ;; Show command for debugging
      (insert (propertize
               (format "[Command was: %s]\n"
                       (mapconcat #'identity (process-command process) " "))
               'face 'font-lock-comment-face))
      ;; Suggest checking claude installation
      (insert (propertize
               "[Hint: Check if 'claude' is installed and in your PATH]\n"
               'face 'font-lock-warning-face))))))

;;; UI Mode

(defvar claude-code-native-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'claude-code-native-send-region-or-prompt)
    (define-key map (kbd "C-c C-p") #'claude-code-native-send-prompt)
    (define-key map (kbd "C-c C-k") #'claude-code-native-kill-process)
    (define-key map (kbd "C-c C-n") #'claude-code-native-new-session)
    map)
  "Keymap for claude-code-native-mode.")

(define-derived-mode claude-code-native-mode markdown-mode "Claude-Native"
  "Major mode for native Claude Code interface."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (read-only-mode -1))

;;; Interactive Commands

(defun claude-code-native-send-region-or-prompt (start end)
  "Send region from START to END, or prompt interactively."
  (interactive "r")
  (if (use-region-p)
      (claude-code-native-send-prompt (buffer-substring-no-properties start end))
    (call-interactively #'claude-code-native-send-prompt)))

(defun claude-code-native-kill-process ()
  "Kill the current Claude process."
  (interactive)
  (when (and claude-code-native-process
             (process-live-p claude-code-native-process))
    (kill-process claude-code-native-process)
    (message "Claude process killed")))

(defun claude-code-native-new-session ()
  "Start a new session, forgetting the previous context."
  (interactive)
  (let ((project-root (project-root (project-current))))
    (remhash project-root claude-code-native-sessions)
    (with-current-buffer (claude-code-native-get-or-create-buffer)
      (setq claude-code-native-session-id nil))
    (claude-code-native-send-prompt "New session started. How can I help you?")))

;;; Entry Point

;;;###autoload
(defun claude-code-native ()
  "Start native Claude Code interface for current project."
  (interactive)
  ;; Check if claude command exists
  (unless (executable-find claude-code-native-claude-command)
    (error "Claude command not found! Please install Claude Code CLI first"))

  (let ((buffer (claude-code-native-get-or-create-buffer)))
    (switch-to-buffer buffer)
    (when (= (point-max) 1)
      ;; New buffer, add welcome message
      (insert (propertize "# Claude Code Native Interface\n\n" 'face 'info-title-1))
      (insert "Project: " (project-root (project-current)) "\n")
      (insert "Session: " (or (claude-code-native-get-session-id) "Will be assigned on first query") "\n\n")
      (insert "Commands:\n")
      (insert "  C-c C-c - Send region or prompt\n")
      (insert "  C-c C-p - Send prompt\n")
      (insert "  C-c C-k - Kill current process\n")
      (insert "  C-c C-n - New session\n\n")
      (insert "Debug mode: " (if claude-code-native-debug "ON" "OFF") "\n")
      (insert "  (setq claude-code-native-debug t) to enable debug output\n\n"))))

(provide 'claude-code-emacs-native)
;;; claude-code-emacs-native.el ends here
