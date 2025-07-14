;;; claude-code-prompt.el --- Prompt file management for Claude Code Emacs -*- lexical-binding: t; -*-

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

;; This module provides prompt file management functionality for Claude Code Emacs.
;; It includes functions for:
;; - Opening and creating project-specific prompt files
;; - Sending prompt sections and regions to Claude Code
;; - Extracting markdown sections for context

;;; Code:

(require 'projectile)
(require 'markdown-mode)

;; Forward declarations
(declare-function claude-code-prompt-mode "claude-code-ui" ())
(declare-function claude-code-normalize-project-root "claude-code-core" (project-root))
(declare-function claude-code-send-string "claude-code-core" (string &optional paste-p))

;;;###autoload
(defun claude-code-open-prompt-file ()
  "Open project-specific prompt file in another window."
  (interactive)
  (let* ((project-root (claude-code-normalize-project-root (projectile-project-root)))
         (prompt-file (expand-file-name ".claude-code.prompt.md" project-root)))
    (unless (file-exists-p prompt-file)
      (with-temp-file prompt-file
        (insert "# Claude Code Prompts for " (file-name-nondirectory (directory-file-name project-root)) "\n\n"
                "This file contains prompts for Claude Code sessions. "
                "It serves as a persistent workspace for your interactions with Claude Code.\n\n"
                "## Key Bindings (claude-code-prompt-mode)\n\n"
                "| Key | Action |\n"
                "|-----|--------|\n"
                "| `C-c C-s` | Send section at point |\n"
                "| `C-c C-r` | Send selected region |\n"
                "| `C-c C-o` | Open Claude Code session |\n"
                "| `C-c C-t` | Open transient menu |\n"
                "| `@` | Complete file path |\n\n"
                "## Example Prompts\n\n"
                "Fix the bug in @src/utils.js where the parser fails on empty strings\n\n"
                "---\n\n"
                "Add unit tests for @src/api/auth.js\n\n"
                "---\n\n"
                "Refactor @src/components/Header.jsx to use React hooks\n\n")))
    (let ((buffer (find-file-noselect prompt-file)))
      (with-current-buffer buffer
        (claude-code-prompt-mode)
        (goto-char (point-max)))
      (switch-to-buffer-other-window buffer))))

(defun claude-code-send-prompt-at-point ()
  "Send the prompt section at point to Claude Code buffer."
  (interactive)
  (let ((section-text (claude-code-get-markdown-section-at-point)))
    (if section-text
        (claude-code-send-string section-text t)
      (message "No markdown section found at point"))))

(defun claude-code-send-prompt-region ()
  "Send the selected region from prompt buffer to Claude Code buffer."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (claude-code-send-string text t))
    (message "No region selected")))

(defun claude-code-get-markdown-section-at-point ()
  "Get the markdown section at point.
Returns text from current heading to next heading or end of buffer."
  (save-excursion
    (let* ((start (progn
                    (if (markdown-heading-at-point)
                        (point)
                      (markdown-previous-heading)
                      (point))))
           (end (progn
                  (goto-char start)
                  (if (markdown-next-heading)
                      (1- (point))
                    (point-max)))))
      (buffer-substring-no-properties start end))))

(defun claude-code--calculate-end-line (position)
  "Calculate the line number at POSITION, adjusting for newline at end.
If POSITION is at the beginning of a line (after a newline),
return the previous line number."
  (save-excursion
    (goto-char position)
    ;; If we're at the beginning of a line (column 0)
    ;; and not at the beginning of the buffer,
    ;; use the previous line number
    (if (and (bolp) (not (bobp)))
        (1- (line-number-at-pos))
      (line-number-at-pos))))

(defun claude-code--format-file-path-with-lines (relative-path start-line end-line)
  "Format RELATIVE-PATH with line numbers.
If START-LINE equals END-LINE, format as @file#L10.
Otherwise, format as @file#L10-15."
  (if (= start-line end-line)
      (format "@%s#L%d" relative-path start-line)
    (format "@%s#L%d-%d" relative-path start-line end-line)))

(defun claude-code--get-relative-path (&optional file)
  "Get the project-relative path for FILE (or current buffer's file).
Returns nil if no file or project root cannot be determined."
  (let* ((target-file (or file (buffer-file-name)))
         (project-root (when target-file
                         (claude-code-normalize-project-root
                          (projectile-project-root (file-name-directory target-file))))))
    (when (and target-file project-root)
      (file-relative-name target-file project-root))))

;;;###autoload
(defun claude-code-insert-region-path-to-prompt ()
  "Insert project-relative path and content of selected region into prompt buffer."
  (interactive)
  (if (use-region-p)
      (let* ((relative-path (claude-code--get-relative-path))
             (region-start (region-beginning))
             (region-end (region-end))
             (start-line (line-number-at-pos region-start))
             (end-line (claude-code--calculate-end-line region-end))
             (region-content (buffer-substring-no-properties region-start region-end)))
        (if relative-path
            (let ((path-with-lines (claude-code--format-file-path-with-lines
                                    relative-path start-line end-line)))
              ;; Find or create the prompt buffer
              (claude-code-open-prompt-file)
              (goto-char (point-max))
              (insert "\n" path-with-lines "\n```\n"
                      region-content
                      (if (string-suffix-p "\n" region-content) "" "\n")
                      "```\n")
              (message "Inserted: %s with content" path-with-lines))
          (message "Cannot determine project-relative path for current buffer")))
    (message "No region selected")))

;;;###autoload
(defun claude-code-insert-current-file-path-to-prompt ()
  "Insert the current file's @-prefixed path into prompt buffer.
If region is selected, append line number range (e.g., @file.el#L10-15)."
  (interactive)
  (let* ((relative-path (claude-code--get-relative-path))
         (has-region (use-region-p))
         (region-start (when has-region (region-beginning)))
         (region-end (when has-region (region-end)))
         (start-line (when has-region (line-number-at-pos region-start)))
         (end-line (when has-region (claude-code--calculate-end-line region-end))))
    (if relative-path
        (let ((at-path (if has-region
                           (claude-code--format-file-path-with-lines
                            relative-path start-line end-line)
                         (concat "@" relative-path))))
          ;; Find or create the prompt buffer
          (claude-code-open-prompt-file)
          (goto-char (point-max))
          (insert "\n" at-path "\n")
          (message "Inserted: %s" at-path))
      (message "Cannot determine project-relative path for current buffer"))))

;;;###autoload
(defun claude-code-insert-current-file-path-to-session ()
  "Insert the current file's @-prefixed path directly into Claude Code session.
If region is selected, append line number range (e.g., @file.el#L10-15)."
  (interactive)
  (let* ((relative-path (claude-code--get-relative-path))
         (has-region (use-region-p))
         (region-start (when has-region (region-beginning)))
         (region-end (when has-region (region-end)))
         (start-line (when has-region (line-number-at-pos region-start)))
         (end-line (when has-region (claude-code--calculate-end-line region-end))))
    (if relative-path
        (let ((at-path (if has-region
                           (claude-code--format-file-path-with-lines
                            relative-path start-line end-line)
                         (concat "@" relative-path))))
          ;; Send directly to Claude Code session
          (claude-code-send-string at-path t)
          (message "Sent to Claude Code: %s" at-path))
      (message "Cannot determine project-relative path for current buffer"))))

(provide 'claude-code-prompt)
;;; claude-code-prompt.el ends here
