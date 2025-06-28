;;; claude-code-emacs-prompt.el --- Prompt file management for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (projectile "2.9.1") (markdown-mode "2.7"))

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
(declare-function claude-code-emacs-send-string "claude-code-emacs-core" (string &optional paste-p))
(declare-function claude-code-emacs-prompt-mode "claude-code-emacs-ui" ())

;;;###autoload
(defun claude-code-emacs-open-prompt-file ()
  "Open project-specific prompt file in another window."
  (interactive)
  (let* ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root)))
         (prompt-file (expand-file-name ".claude-code-emacs.prompt.md" project-root)))
    (unless (file-exists-p prompt-file)
      (with-temp-file prompt-file
        (insert "# Claude Code Prompts for " (file-name-nondirectory (directory-file-name project-root)) "\n\n"
                "This file contains prompts for Claude Code sessions in this project.\n\n"
                "## Project Context\n\n"
                "## Common Tasks\n\n"
                "## Code Patterns\n\n")))
    (switch-to-buffer-other-window (find-file-noselect prompt-file))
    (claude-code-emacs-prompt-mode)))

(defun claude-code-emacs-send-prompt-at-point ()
  "Send the prompt section at point to Claude Code buffer."
  (interactive)
  (let ((section-text (claude-code-emacs-get-markdown-section-at-point)))
    (if section-text
        (claude-code-emacs-send-string section-text t)
      (message "No markdown section found at point"))))

(defun claude-code-emacs-send-prompt-region ()
  "Send the selected region from prompt buffer to Claude Code buffer."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties (region-beginning) (region-end))))
        (claude-code-emacs-send-string text t))
    (message "No region selected")))

(defun claude-code-emacs-get-markdown-section-at-point ()
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

(defun claude-code-emacs--calculate-end-line (position)
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

(defun claude-code-emacs--format-file-path-with-lines (relative-path start-line end-line)
  "Format RELATIVE-PATH with line numbers.
If START-LINE equals END-LINE, format as @file#L10.
Otherwise, format as @file#L10-15."
  (if (= start-line end-line)
      (format "@%s#L%d" relative-path start-line)
    (format "@%s#L%d-%d" relative-path start-line end-line)))

(defun claude-code-emacs--get-relative-path (&optional file)
  "Get the project-relative path for FILE (or current buffer's file).
Returns nil if no file or project root cannot be determined."
  (let* ((target-file (or file (buffer-file-name)))
         (project-root (when target-file
                        (claude-code-emacs-normalize-project-root
                         (projectile-project-root (file-name-directory target-file))))))
    (when (and target-file project-root)
      (file-relative-name target-file project-root))))

;;;###autoload
(defun claude-code-emacs-insert-region-path-to-prompt ()
  "Insert the project-relative path and content of the selected region into prompt buffer."
  (interactive)
  (if (use-region-p)
      (let* ((relative-path (claude-code-emacs--get-relative-path))
             (region-start (region-beginning))
             (region-end (region-end))
             (start-line (line-number-at-pos region-start))
             (end-line (claude-code-emacs--calculate-end-line region-end))
             (region-content (buffer-substring-no-properties region-start region-end)))
        (if relative-path
            (let ((path-with-lines (claude-code-emacs--format-file-path-with-lines
                                   relative-path start-line end-line)))
              ;; Find or create the prompt buffer
              (claude-code-emacs-open-prompt-file)
              (goto-char (point-max))
              (insert "\n" path-with-lines "\n```\n"
                      region-content
                      (if (string-suffix-p "\n" region-content) "" "\n")
                      "```\n")
              (message "Inserted: %s with content" path-with-lines))
          (message "Cannot determine project-relative path for current buffer")))
    (message "No region selected")))

;;;###autoload
(defun claude-code-emacs-insert-current-file-path-to-prompt ()
  "Insert the current file's @-prefixed path into prompt buffer.
If region is selected, append line number range (e.g., @file.el#L10-15)."
  (interactive)
  (let* ((relative-path (claude-code-emacs--get-relative-path))
         (has-region (use-region-p))
         (region-start (when has-region (region-beginning)))
         (region-end (when has-region (region-end)))
         (start-line (when has-region (line-number-at-pos region-start)))
         (end-line (when has-region (claude-code-emacs--calculate-end-line region-end))))
    (if relative-path
        (let ((at-path (if has-region
                          (claude-code-emacs--format-file-path-with-lines
                           relative-path start-line end-line)
                        (concat "@" relative-path))))
          ;; Find or create the prompt buffer
          (claude-code-emacs-open-prompt-file)
          (goto-char (point-max))
          (insert "\n" at-path "\n")
          (message "Inserted: %s" at-path))
      (message "Cannot determine project-relative path for current buffer"))))

(provide 'claude-code-emacs-prompt)
;;; claude-code-emacs-prompt.el ends here
