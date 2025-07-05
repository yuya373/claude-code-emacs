;;; test-native-demo.el --- Demo for native Claude Code interface -*- lexical-binding: t; -*-

;;; Commentary:
;; Simple demo to test the native interface prototype

;;; Code:

(add-to-list 'load-path (expand-file-name "." default-directory))
(require 'claude-code-emacs-native)

;; Demo function
(defun claude-code-native-demo ()
  "Run a demo of the native interface."
  (interactive)
  ;; Start the interface
  (claude-code-native)
  
  ;; Give user instructions
  (message "Native Claude Code interface started! Try:")
  (message "  1. Type a prompt and press C-c C-c")
  (message "  2. Select text and press C-c C-c to send it")
  (message "  3. Press C-c C-p for a new prompt")
  (message "  4. Press C-c C-k to stop a running query"))

;; Quick test function
(defun claude-code-native-test-stream-parsing ()
  "Test the JSON stream parsing."
  (let ((test-lines
         '("{\"type\":\"message_start\",\"message\":{\"id\":\"msg_123\"}}"
           "{\"type\":\"content_block_start\",\"content_block\":{\"type\":\"text\"}}"
           "{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\"Hello\"}}"
           "{\"type\":\"content_block_delta\",\"delta\":{\"type\":\"text_delta\",\"text\":\" world!\"}}"
           "{\"type\":\"message_stop\"}")))
    (with-temp-buffer
      (claude-code-native-mode)
      (dolist (line test-lines)
        (claude-code-native-handle-json-line line))
      (buffer-string))))

;; Example slash command emulation
(defun claude-code-native-slash-init ()
  "Emulate /init command by reading instructions."
  (interactive)
  (let ((instructions-file (expand-file-name ".claude/instructions.md" 
                                             (project-root (project-current)))))
    (if (file-exists-p instructions-file)
        (let ((content (with-temp-buffer
                         (insert-file-contents instructions-file)
                         (buffer-string))))
          (claude-code-native-send-prompt 
           (format "Please read and follow these project instructions:\n\n%s" content)))
      (message "No .claude/instructions.md found in project"))))

;; Example custom command
(defun claude-code-native-send-file (filename)
  "Send a file's contents to Claude."
  (interactive "fFile: ")
  (if (file-exists-p filename)
      (let ((content (with-temp-buffer
                       (insert-file-contents filename)
                       (buffer-string))))
        (claude-code-native-send-prompt
         (format "Here is the content of %s:\n\n```\n%s\n```\n\nPlease analyze this file."
                 (file-name-nondirectory filename)
                 content)))
    (message "File not found: %s" filename)))

;; Test command availability
(defun claude-code-native-check-setup ()
  "Check if Claude Code CLI is properly set up."
  (interactive)
  (let ((claude-path (executable-find "claude")))
    (if claude-path
        (progn
          (message "✅ Claude found at: %s" claude-path)
          ;; Try to get version
          (let ((version-output (shell-command-to-string "claude --version 2>&1")))
            (message "Version: %s" (string-trim version-output))))
      (message "❌ Claude not found in PATH!")
      (message "Please install Claude Code CLI first:")
      (message "  npm install -g @anthropic-ai/claude-cli"))))

;; Debug helper
(defun claude-code-native-toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (setq claude-code-native-debug (not claude-code-native-debug))
  (message "Debug mode: %s" (if claude-code-native-debug "ON" "OFF")))

(provide 'test-native-demo)
;;; test-native-demo.el ends here