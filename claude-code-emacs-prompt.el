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

(provide 'claude-code-emacs-prompt)
;;; claude-code-emacs-prompt.el ends here
