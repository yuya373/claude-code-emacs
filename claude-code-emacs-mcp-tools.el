;;; claude-code-emacs-mcp-tools.el --- MCP tool handlers for Claude Code Emacs -*- lexical-binding: t; -*-

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

;; This module implements the MCP tool handlers:
;; - getOpenBuffers: List open buffers in project
;; - getCurrentSelection: Get current text selection
;; - getDiagnostics: Get LSP diagnostics
;; - sendNotification: Send notifications to user via alert

;;; Code:

(require 'claude-code-emacs-core)
(require 'projectile)
(require 'lsp-mode nil t)
(require 'lsp-protocol nil t)
(require 'vc)
(require 'ediff)
(require 'alert nil t)  ;; Optional dependency

;; LSP function declarations
(declare-function lsp-diagnostics "lsp-mode" (&optional all-workspaces))
(declare-function lsp:diagnostic-range "lsp-protocol" (diagnostic))
(declare-function lsp:range-start "lsp-protocol" (range))
(declare-function lsp:position-line "lsp-protocol" (position))
(declare-function lsp:position-character "lsp-protocol" (position))
(declare-function lsp:diagnostic-severity? "lsp-protocol" (diagnostic))
(declare-function lsp:diagnostic-source? "lsp-protocol" (diagnostic))
(declare-function lsp:diagnostic-message "lsp-protocol" (diagnostic))
(declare-function lsp-request "lsp-mode" (method &optional params))
(declare-function lsp--text-document-position-params "lsp-mode" ())
(declare-function lsp--location-uri "lsp-mode" (location))
(declare-function lsp--location-range "lsp-mode" (location))
(declare-function lsp--uri-to-path "lsp-mode" (uri))
(declare-function lsp:range-end "lsp-protocol" (range))
(declare-function lsp:location-uri "lsp-protocol" (location))
(declare-function lsp:location-range "lsp-protocol" (location))
(declare-function lsp:hover-contents "lsp-protocol" (hover))
(declare-function lsp-markup-content? "lsp-protocol" (obj))
(declare-function lsp:markup-content-value "lsp-protocol" (content))
(declare-function lsp-marked-string? "lsp-protocol" (obj))
(declare-function lsp:marked-string-language "lsp-protocol" (marked-string))
(declare-function lsp:marked-string-value "lsp-protocol" (marked-string))

;;; MCP Tool Handlers


(defun claude-code-emacs-mcp-handle-getOpenBuffers (params)
  "Handle getOpenBuffers request with PARAMS."
  (let* ((include-hidden (cdr (assoc 'includeHidden params)))
         (project-root (claude-code-emacs-normalize-project-root (projectile-project-root)))
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
                  (active . ,(if (eq buffer (current-buffer)) t json-false))
                  (modified . ,(if (buffer-modified-p buffer) t json-false)))
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
  "Handle getDiagnostics request with PARAMS.
Returns project-wide diagnostics using specified buffer for LSP context."
  (condition-case err
      (let ((diagnostics '())
            (buffer-name (cdr (assoc 'buffer params))))

        ;; Buffer parameter is required for LSP context
        (unless buffer-name
          (error "Buffer name is required for LSP context"))

        ;; Check buffer exists first
        (unless (get-buffer buffer-name)
          (error "Buffer %s not found for LSP context" buffer-name))

        (when (fboundp 'lsp-diagnostics)
          (let ((lsp-diags (with-current-buffer (get-buffer buffer-name)
                             (condition-case diag-err
                                 (lsp-diagnostics t)
                               (error
                                (message "Error getting LSP diagnostics in buffer %s: %s"
                                         buffer-name
                                         (error-message-string diag-err))
                                nil)))))
            (when lsp-diags
              (maphash (lambda (file diags)
                         (dolist (diag diags)
                           (let* ((message (lsp:diagnostic-message diag))
                                  (severity (lsp:diagnostic-severity? diag))
                                  (source (lsp:diagnostic-source? diag))
                                  (range (lsp:diagnostic-range diag))
                                  (start (lsp:range-start range))
                                  (line (1+ (lsp:position-line start)))
                                  (column (lsp:position-character start)))
                             (when (and file line)
                               (push `((file . ,file)
                                       (line . ,line)
                                       (column . ,(or column 0))
                                       (severity . ,(pcase severity
                                                      (1 "error")
                                                      (2 "warning")
                                                      (3 "information")
                                                      (4 "hint")
                                                      (5 "max")
                                                      (_ "")))
                                       (message . ,message)
                                       (source . ,(or source "lsp")))
                                     diagnostics)))))
                       lsp-diags)))
          `((diagnostics . ,(nreverse diagnostics)))))
    (error
     ;; Log error and re-throw it
     (message "Error in getDiagnostics handler: %s" (error-message-string err))
     (signal (car err) (cdr err)))))

;;; Diff Tool Handlers

(defun claude-code-emacs-mcp-handle-openDiffFile (params)
  "Handle openDiffFile request with PARAMS."
  (let ((file-a (cdr (assoc 'fileA params)))
        (file-b (cdr (assoc 'fileB params))))
    (condition-case err
        (progn
          (unless (and file-a file-b)
            (error "Missing required parameters: fileA and fileB"))
          (let ((path-a (expand-file-name file-a (claude-code-emacs-normalize-project-root (projectile-project-root))))
                (path-b (expand-file-name file-b (claude-code-emacs-normalize-project-root (projectile-project-root)))))
            (unless (file-exists-p path-a)
              (error "File not found: %s" path-a))
            (unless (file-exists-p path-b)
              (error "File not found: %s" path-b))
            (ediff-files path-a path-b))
          `((status . "success")
            (message . "Opened ediff session")))
      (error
       `((status . "error")
         (message . ,(error-message-string err)))))))

(defun claude-code-emacs-mcp-handle-openRevisionDiff (params)
  "Handle openRevisionDiff request with PARAMS."
  (let ((file (cdr (assoc 'file params)))
        (revision (or (cdr (assoc 'revision params)) "HEAD")))
    (condition-case err
        (progn
          (unless file
            (error "Missing required parameter: file"))
          (let ((full-path (expand-file-name file (claude-code-emacs-normalize-project-root (projectile-project-root)))))
            (unless (file-exists-p full-path)
              (error "File not found: %s" full-path))
            ;; Open the file and compare with revision
            (condition-case inner-err
                (with-current-buffer (find-file-noselect full-path)
                  ;; Check if file is under version control
                  (let ((backend (vc-backend buffer-file-name)))
                    (unless backend
                      (error "File is not under version control: %s" file))
                    ;; Use vc-version-ediff for version-controlled files
                    (vc-version-ediff (list buffer-file-name) revision nil)))
              (error
               (message "Inner error in openRevisionDiff: %s" (error-message-string inner-err))
               (signal (car inner-err) (cdr inner-err)))))
          `((status . "success")
            (message . "Opened revision diff")))
      (error
       (message "Error in openRevisionDiff: %s" (error-message-string err))
       `((status . "error")
         (message . ,(error-message-string err)))))))

(defun claude-code-emacs-mcp-handle-openCurrentChanges (params)
  "Handle openCurrentChanges request with PARAMS."
  (let ((file (cdr (assoc 'file params))))
    (condition-case err
        (let ((target-file (if file
                               (expand-file-name file (claude-code-emacs-normalize-project-root (projectile-project-root)))
                             (buffer-file-name))))
          (unless target-file
            (error "No file specified and current buffer has no file"))
          (unless (file-exists-p target-file)
            (error "File not found: %s" target-file))
          ;; Use vc-diff for showing uncommitted changes
          (with-current-buffer (find-file-noselect target-file)
            (vc-diff nil t))
          `((status . "success")
            (message . "Showing changes")
            (file . ,(file-name-nondirectory target-file))))
      (error
       `((status . "error")
         (message . ,(error-message-string err)))))))

(defun claude-code-emacs-mcp-handle-openDiffContent (params)
  "Handle openDiffContent request with PARAMS."
  (let ((content-a (cdr (assoc 'contentA params)))
        (content-b (cdr (assoc 'contentB params)))
        (title-a (cdr (assoc 'titleA params)))
        (title-b (cdr (assoc 'titleB params))))
    (condition-case err
        (progn
          (unless (and content-a content-b title-a title-b)
            (error "Missing required parameters: contentA, contentB, titleA, and titleB"))
          (let* ((buf-a (get-buffer-create title-a))
                 (buf-b (get-buffer-create title-b)))
            ;; Set up buffer A
            (with-current-buffer buf-a
              (erase-buffer)
              (insert content-a)
              (goto-char (point-min)))
            ;; Set up buffer B
            (with-current-buffer buf-b
              (erase-buffer)
              (insert content-b)
              (goto-char (point-min)))
            ;; Start ediff
            (ediff-buffers buf-a buf-b))
          `((status . "success")
            (message . ,(format "Opened ediff session for buffers: %s and %s" title-a title-b))))
      (error
       `((status . "error")
         (message . ,(error-message-string err)))))))

;;; Resource Handlers

(defun claude-code-emacs-mcp-handle-get-buffer-content (params)
  "Get content of a buffer specified by PATH in PARAMS."
  (let* ((path (cdr (assoc 'path params)))
         (full-path (expand-file-name path (claude-code-emacs-normalize-project-root (projectile-project-root)))))
    (if (file-exists-p full-path)
        (let ((buffer (find-buffer-visiting full-path)))
          (if buffer
              ;; Return buffer content (which may include unsaved changes)
              `((success . t)
                (content . ,(with-current-buffer buffer
                              (buffer-substring-no-properties (point-min) (point-max)))))
            ;; File exists but not opened, read from disk
            `((success . t)
              (content . ,(with-temp-buffer
                            (insert-file-contents full-path)
                            (buffer-string))))))
      `((success . nil)
        (error . ,(format "File not found: %s" full-path))))))

(defun claude-code-emacs-mcp-handle-get-project-info (_params)
  "Get project information."
  (let ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root))))
    `((success . t)
      (projectRoot . ,project-root)
      (projectName . ,(projectile-project-name))
      (projectType . ,(projectile-project-type))
      (vcs . ,(when (vc-responsible-backend project-root)
                (symbol-name (vc-responsible-backend project-root))))
      (branch . ,(when (vc-responsible-backend project-root)
                   (ignore-errors
                     (vc-working-revision project-root))))
      (lastModified . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z")))))

(defun claude-code-emacs-mcp-handle-get-project-files (_params)
  "Get list of project files."
  (let ((files (projectile-current-project-files)))
    `((success . t)
      (files . ,(mapcar (lambda (file)
                          `((path . ,file)
                            (relativePath . ,file)
                            (absolutePath . ,(expand-file-name file (claude-code-emacs-normalize-project-root (projectile-project-root))))))
                        files)))))


(defun claude-code-emacs-mcp-handle-get-open-buffers (params)
  "Handle get-open-buffers request for resources with PARAMS."
  (claude-code-emacs-mcp-handle-getOpenBuffers params))


;;; Definition finding

(defun claude-code-emacs-mcp-handle-getDefinition (params)
  "Handle getDefinition request with PARAMS using LSP.
PARAMS must include \\='file\\=', \\='line\\=', and \\='symbol\\=' parameters."
  (let* ((symbol-name (cdr (assoc 'symbol params)))
         (file-path (cdr (assoc 'file params)))
         (line (cdr (assoc 'line params)))
         (definitions '()))

    (condition-case err
        (progn
          ;; Check if LSP is available
          (unless (and (fboundp 'lsp-mode)
                       (fboundp 'lsp-request))
            (error "LSP mode is not available.  Please install and configure lsp-mode"))

          ;; file-path, line, and symbol are required
          (unless file-path
            (error "Missing required parameter: file"))
          (unless line
            (error "Missing required parameter: line"))
          (unless symbol-name
            (error "Missing required parameter: symbol"))

          ;; Visit the specified file
          (let* ((full-path (expand-file-name file-path (claude-code-emacs-normalize-project-root (projectile-project-root))))
                 (buffer (find-file-noselect full-path)))
            (with-current-buffer buffer
              ;; Check if LSP is active in this buffer
              (unless (bound-and-true-p lsp-mode)
                (error "LSP is not active in buffer: %s" (buffer-name)))

              ;; Go to specified position
              (goto-char (point-min))
              (forward-line (1- line))

              ;; Search for the symbol on this line
              (let ((line-end (line-end-position)))
                (if (search-forward symbol-name line-end t)
                    (goto-char (match-beginning 0))
                  ;; If not found, just go to beginning of line
                  (beginning-of-line)))

              ;; Get definitions using lsp-request
              (setq definitions (claude-code-emacs-mcp-get-lsp-definitions-with-request))))

          ;; Return results
          (if definitions
              `((definitions . ,definitions)
                (method . "lsp"))
            (error "No definition found using LSP")))

      (error
       (error "Failed to find definition: %s" (error-message-string err))))))

(defun claude-code-emacs-mcp-get-lsp-definitions-with-request ()
  "Get definitions using `lsp-request'."
  (require 'lsp-mode)
  (condition-case _
      (let* ((params (lsp--text-document-position-params))
             (response (lsp-request "textDocument/definition" params))
             (definitions '()))

        ;; Response can be Location, Location[], LocationLink, or LocationLink[]
        (when response
          (let ((locations (if (listp response)
                               (if (and (listp (car response))
                                        (assoc 'uri (car response)))
                                   ;; Array of locations
                                   response
                                 ;; Single location in a list
                                 (if (assoc 'uri response)
                                     (list response)
                                   ;; Might be wrapped
                                   response))
                             ;; Single location
                             (list response))))
            (dolist (loc locations)
              (let* ((uri (lsp--location-uri loc))
                     (range (lsp--location-range loc))
                     (file (when uri (lsp--uri-to-path uri))))

                (when (and file range)
                  (let* ((def-info (claude-code-emacs-mcp-get-definition-info-at file range)))
                    (when def-info
                      (push def-info definitions))))))))

        (nreverse definitions))

    (error
     ;; Return empty list on error
     nil)))

(defun claude-code-emacs-mcp-get-definition-info-at (file range)
  "Get definition information at FILE with RANGE."
  (condition-case nil
      (let* ((preview (claude-code-emacs-mcp-get-preview-text file range)))
        `((file . ,file)
          (preview . ,preview)
          (range . ,range)))
    (error nil)))

(defun claude-code-emacs-mcp-get-preview-text (file-path range)
  "Get preview text around RANGE with 3 lines of context from FILE-PATH."
  (condition-case nil
      (with-temp-buffer
        (insert-file-contents file-path)
        (claude-code-emacs-mcp-get-preview-text-internal range))
    (error "")))

(defun claude-code-emacs-mcp-get-preview-text-internal (range)
  "Internal function to get preview text for RANGE.
Assumes we're in the correct buffer."
  (let* ((start-line (lsp:position-line (lsp:range-start range)))
         (end-line (lsp:position-line (lsp:range-end range)))
         ;; Calculate preview range with 3 lines padding
         (preview-start-line (max 0 (- start-line 3)))
         (preview-end-line (+ end-line 3))
         preview-start preview-end)

    ;; Move to preview start
    (goto-char (point-min))
    (forward-line preview-start-line)
    (beginning-of-line)
    (setq preview-start (point))

    ;; Move to preview end
    (goto-char (point-min))
    (forward-line preview-end-line)
    (end-of-line)
    (setq preview-end (point))

    ;; Ensure we don't go past buffer boundaries
    (buffer-substring-no-properties
     preview-start
     (min preview-end (point-max)))))

;;; Reference finding

(defun claude-code-emacs-mcp-handle-findReferences (params)
  "Handle findReferences request with PARAMS using LSP.
PARAMS must include \\='file\\=', \\='line\\=', \\='symbol\\=' parameters.
Optional: \\='includeDeclaration\\=' (boolean)."
  (let* ((file-path (cdr (assoc 'file params)))
         (line (cdr (assoc 'line params)))
         (symbol-name (cdr (assoc 'symbol params)))
         (include-declaration (cdr (assoc 'includeDeclaration params)))
         (references '()))

    (condition-case err
        (progn
          ;; Check if LSP is available
          (unless (and (fboundp 'lsp-mode)
                       (fboundp 'lsp-request))
            (error "LSP mode is not available"))

          ;; Validate required parameters
          (unless file-path
            (error "File parameter is required"))
          (unless line
            (error "Line parameter is required"))
          (unless symbol-name
            (error "Symbol parameter is required"))

          ;; Find the file
          (let* ((project-root (claude-code-emacs-normalize-project-root (projectile-project-root)))
                 (absolute-path (expand-file-name file-path project-root))
                 (buffer (find-file-noselect absolute-path)))

            (with-current-buffer buffer
              ;; Move to the specified position
              (goto-char (point-min))
              (forward-line (1- line))

              ;; Search for the symbol on this line
              (let ((line-end (line-end-position)))
                (if (search-forward symbol-name line-end t)
                    (goto-char (match-beginning 0))
                  ;; If not found, just go to beginning of line
                  (beginning-of-line)))

              ;; Check if LSP is active in this buffer
              (unless (bound-and-true-p lsp-mode)
                (error "LSP is not active in this buffer"))

              ;; Find references using LSP
              (let* ((context `(:includeDeclaration ,(if include-declaration t :json-false)))
                     (params (append (lsp--text-document-position-params)
                                     `(:context ,context)))
                     (lsp-response (lsp-request "textDocument/references" params)))

                (when lsp-response
                  (setq references (claude-code-emacs-mcp-convert-lsp-references lsp-response project-root))))))

          ;; Return the references
          `((references . ,references)
            (count . ,(length references))))

      (error
       `((error . ,(error-message-string err)))))))

(defun claude-code-emacs-mcp-convert-lsp-references (lsp-references project-root)
  "Convert LSP-REFERENCES to MCP format relative to PROJECT-ROOT."
  (mapcar (lambda (location)
            (let* ((uri (lsp:location-uri location))
                   (range (lsp:location-range location))
                   (file-path (lsp--uri-to-path uri))
                   (relative-path (file-relative-name file-path project-root)))
              ;; Get preview text for this reference
              (let ((preview (claude-code-emacs-mcp-get-preview-text file-path range)))
                `((file . ,relative-path)
                  (absolutePath . ,file-path)
                  (range . ,range)
                  (preview . ,preview)))))
          lsp-references))

;;; Symbol description

(defun claude-code-emacs-mcp-handle-describeSymbol (params)
  "Handle describeSymbol request with PARAMS using LSP hover.
PARAMS must include \\='file\\=', \\='line\\=', and \\='symbol\\=' parameters."
  (let* ((symbol-name (cdr (assoc 'symbol params)))
         (file-path (cdr (assoc 'file params)))
         (line (cdr (assoc 'line params)))
         (description nil))

    (condition-case err
        (progn
          ;; Check if LSP is available
          (unless (and (fboundp 'lsp-mode)
                       (fboundp 'lsp-request))
            (error "LSP mode is not available.  Please install and configure lsp-mode"))

          ;; file-path, line, and symbol are required
          (unless file-path
            (error "Missing required parameter: file"))
          (unless line
            (error "Missing required parameter: line"))
          (unless symbol-name
            (error "Missing required parameter: symbol"))

          ;; Visit the specified file
          (let* ((full-path (expand-file-name file-path (claude-code-emacs-normalize-project-root (projectile-project-root))))
                 (buffer (find-file-noselect full-path)))
            (with-current-buffer buffer
              ;; Check if LSP is active in this buffer
              (unless (bound-and-true-p lsp-mode)
                (error "LSP is not active in buffer: %s" (buffer-name)))

              ;; Go to specified position
              (goto-char (point-min))
              (forward-line (1- line))

              ;; Search for the symbol on this line
              (let ((line-end (line-end-position)))
                (if (search-forward symbol-name line-end t)
                    (goto-char (match-beginning 0))
                  ;; If not found, just go to beginning of line
                  (beginning-of-line)))

              ;; Get hover information using lsp-request
              (setq description (claude-code-emacs-mcp-get-lsp-hover-info))))

          ;; Return results
          (if description
              `((description . ,description)
                (method . "lsp"))
            (error "No hover information found using LSP")))

      (error
       (error "Failed to describe symbol: %s" (error-message-string err))))))

(defun claude-code-emacs-mcp-get-lsp-hover-info ()
  "Get hover information using `lsp-request'."
  (require 'lsp-mode)
  (condition-case nil
      (let* ((params (lsp--text-document-position-params))
             (response (lsp-request "textDocument/hover" params)))

        (when response
          (let* ((contents (or (lsp:hover-contents response) nil))
                 (documentation nil))

            ;; Process contents - can be string, MarkupContent, or MarkedString[]
            (cond
             ;; Simple string
             ((stringp contents)
              (setq documentation contents))

             ;; MarkupContent with kind and value
             ((lsp-markup-content? contents)
              (setq documentation (lsp:markup-content-value contents)))

             ;; Array of MarkedString
             (t
              (let ((doc-parts '()))
                (dolist (item contents)
                  (cond
                   ;; MarkedString with language and value
                   ((lsp-marked-string? item)
                    (let ((lang (lsp:marked-string-language item))
                          (value (lsp:marked-string-value item)))
                      ;; Format as markdown code block
                      (push (format "```%s\n%s\n```" lang value) doc-parts)))
                   ;; Simple string
                   ((stringp item)
                    (push item doc-parts))))
                (when doc-parts
                  (setq documentation (string-join (nreverse doc-parts) "\n\n"))))))

            ;; Return only documentation
            (when documentation
              `((documentation . ,documentation))))))

    (error nil)))

;;; Notification handler

(defun claude-code-emacs-mcp-handle-sendNotification (params)
  "Handle sendNotification request with PARAMS.
PARAMS should include \\='title\\=' and \\='message\\='."
  (let ((title (cdr (assoc 'title params)))
        (message-text (cdr (assoc 'message params))))

    ;; Validate required parameters
    (unless title
      (error "Title is required"))
    (unless message-text
      (error "Message is required"))

    ;; Send notification using alert if available, otherwise use message
    (if (fboundp 'alert)
        (alert message-text
               :title title
               :category 'claude-code)
      ;; Fallback to message if alert is not available
      (message "[%s] %s" title message-text))

    ;; Return success response
    `((success . t)
      (message . "Notification sent"))))

(provide 'claude-code-emacs-mcp-tools)
;;; claude-code-emacs-mcp-tools.el ends here
