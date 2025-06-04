;;; claude-code-emacs-mcp-tools.el --- MCP tool handlers for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: DESKTOP2 <yuya373@DESKTOP2>
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (projectile "2.9.1") (lsp-mode "9.0.0"))

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

;; This module implements the MCP tool handlers:
;; - openFile: Open files with optional text selection
;; - getOpenBuffers: List open buffers in project
;; - getCurrentSelection: Get current text selection
;; - getDiagnostics: Get LSP diagnostics

;;; Code:

(require 'projectile)
(require 'lsp-mode nil t)
(require 'lsp-protocol nil t)

;;; MCP Tool Handlers

(defun claude-code-emacs-mcp-handle-openFile (params)
  "Handle openFile request with PARAMS."
  (let* ((path (cdr (assoc 'path params)))
         (start-text (cdr (assoc 'startText params)))
         (end-text (cdr (assoc 'endText params)))
         (full-path (expand-file-name path (projectile-project-root))))
    (message "Params: %s" params)

    (unless (file-exists-p full-path)
      (signal 'file-missing (list "File not found" full-path)))

    (find-file-other-window full-path)

    ;; Handle text selection if specified
    (when (and start-text end-text)
      (goto-char (point-min))
      (when (search-forward start-text nil t)
        (let ((start (match-beginning 0)))
          (when (search-forward end-text nil t)
            (let ((end (point)))
              (goto-char start)
              (set-mark end))))))

    `((success . t)
      (path . ,full-path))))

(defun claude-code-emacs-mcp-handle-getOpenBuffers (params)
  "Handle getOpenBuffers request with PARAMS."
  (let* ((include-hidden (cdr (assoc 'includeHidden params)))
         (project-root (projectile-project-root))
         (buffers '()))

    (dolist (buffer (buffer-list))
      (let ((file-path (buffer-file-name buffer))
            (buffer-name (buffer-name buffer)))
        (when (and file-path
                   (string-prefix-p project-root file-path)
                   (or include-hidden
                       (not (string-prefix-p " " buffer-name))))
          (push `((path . ,file-path)
                  (name . ,buffer-name)
                  (active . ,(eq buffer (current-buffer)))
                  (modified . ,(buffer-modified-p buffer)))
                buffers))))

    `((buffers . ,(nreverse buffers)))))

(defun claude-code-emacs-mcp-handle-getCurrentSelection (_params)
  "Handle getCurrentSelection request."
  (if (use-region-p)
      (let* ((start (region-beginning))
             (end (region-end))
             (text (buffer-substring-no-properties start end))
             (start-line (line-number-at-pos start))
             (end-line (line-number-at-pos end))
             (start-char (save-excursion
                           (goto-char start)
                           (current-column)))
             (end-char (save-excursion
                         (goto-char end)
                         (current-column))))
        `((text . ,text)
          (startLine . ,start-line)
          (endLine . ,end-line)
          (startChar . ,start-char)
          (endChar . ,end-char)
          (fileName . ,(or (buffer-file-name) ""))))
    `((text . "")
      (startLine . 0)
      (endLine . 0)
      (startChar . 0)
      (endChar . 0)
      (fileName . ""))))

(defun claude-code-emacs-mcp-handle-getDiagnostics (params)
  "Handle getDiagnostics request with PARAMS."
  (condition-case nil
      (let* ((buffer-path (cdr (assoc 'bufferPath params)))
             (diagnostics '()))

        (when (and (fboundp 'lsp-diagnostics)
                   (fboundp 'lsp:diagnostic-message))
          (let ((lsp-diags (condition-case nil
                               (if buffer-path
                                   (let ((buffer (find-buffer-visiting buffer-path)))
                                     (when buffer
                                       (with-current-buffer buffer
                                         (lsp-diagnostics))))
                                 (lsp-diagnostics))
                             (error nil))))
            (when lsp-diags
              (maphash
               (lambda (file diags-by-line)
                 (maphash
                  (lambda (line diags)
                    (dolist (diag diags)
                      (push `((file . ,file)
                              (line . ,line)
                              (column . ,(if (and (fboundp 'lsp:position-character)
                                                  (fboundp 'lsp:range-start)
                                                  (fboundp 'lsp:diagnostic-range))
                                             (or (lsp:position-character
                                                  (lsp:range-start
                                                   (lsp:diagnostic-range diag)))
                                                 0)
                                           0))
                              (severity . ,(if (fboundp 'lsp:diagnostic-severity)
                                               (pcase (lsp:diagnostic-severity diag)
                                                 (1 "error")
                                                 (2 "warning")
                                                 (_ "info"))
                                             "info"))
                              (message . ,(lsp:diagnostic-message diag))
                              (source . ,(if (fboundp 'lsp:diagnostic-source)
                                             (or (lsp:diagnostic-source diag) "lsp")
                                           "lsp")))
                            diagnostics)))
                  diags-by-line))
               lsp-diags))))

        `((diagnostics . ,(nreverse diagnostics))))
    (error
     ;; Return empty diagnostics list on any error
     `((diagnostics . ())))))

(provide 'claude-code-emacs-mcp-tools)
;;; claude-code-emacs-mcp-tools.el ends here
