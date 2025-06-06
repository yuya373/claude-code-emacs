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
(require 'vc)
(require 'ediff)

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

(defun claude-code-emacs-mcp-handle-getDiagnostics (_params)
  "Handle getDiagnostics request.
Always returns project-wide diagnostics."
  (condition-case nil
      (let ((diagnostics '()))

        (when (and (fboundp 'lsp-diagnostics)
                   (fboundp 'lsp:diagnostic-message))
          (let ((lsp-diags (condition-case nil
                               (lsp-diagnostics)
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

;;; Diff Tool Handlers

(defun claude-code-emacs-mcp-handle-openDiff (params)
  "Handle openDiff request with PARAMS."
  (let ((mode (or (cdr (assoc 'mode params)) "files"))
        (file-a (cdr (assoc 'fileA params)))
        (file-b (cdr (assoc 'fileB params)))
        (buffer-a (cdr (assoc 'bufferA params)))
        (buffer-b (cdr (assoc 'bufferB params))))
    (condition-case err
        (progn
          (cond
           ((string= mode "files")
            (unless (and file-a file-b)
              (error "Missing required parameters: fileA and fileB"))
            (let ((path-a (expand-file-name file-a (projectile-project-root)))
                  (path-b (expand-file-name file-b (projectile-project-root))))
              (unless (file-exists-p path-a)
                (error "File not found: %s" path-a))
              (unless (file-exists-p path-b)
                (error "File not found: %s" path-b))
              (ediff-files path-a path-b)))
           ((string= mode "buffers")
            (unless (and buffer-a buffer-b)
              (error "Missing required parameters: bufferA and bufferB"))
            (unless (get-buffer buffer-a)
              (error "Buffer not found: %s" buffer-a))
            (unless (get-buffer buffer-b)
              (error "Buffer not found: %s" buffer-b))
            (ediff-buffers buffer-a buffer-b))
           (t
            (error "Unsupported diff mode: %s" mode)))
          `((status . "success")
            (message . "Opened ediff session")))
      (error
       `((status . "error")
         (message . ,(error-message-string err)))))))

(defun claude-code-emacs-mcp-handle-openDiff3 (params)
  "Handle openDiff3 request with PARAMS."
  (let ((file-a (cdr (assoc 'fileA params)))
        (file-b (cdr (assoc 'fileB params)))
        (file-c (cdr (assoc 'fileC params)))
        (ancestor (cdr (assoc 'ancestor params))))
    (condition-case err
        (progn
          (unless (and file-a file-b file-c)
            (error "Missing required parameters: fileA, fileB, and fileC"))
          (let ((path-a (expand-file-name file-a (projectile-project-root)))
                (path-b (expand-file-name file-b (projectile-project-root)))
                (path-c (expand-file-name file-c (projectile-project-root))))
            (unless (file-exists-p path-a)
              (error "File not found: %s" path-a))
            (unless (file-exists-p path-b)
              (error "File not found: %s" path-b))
            (unless (file-exists-p path-c)
              (error "File not found: %s" path-c))
            (if ancestor
                (let ((ancestor-path (expand-file-name ancestor (projectile-project-root))))
                  (unless (file-exists-p ancestor-path)
                    (error "Ancestor file not found: %s" ancestor-path))
                  (ediff-merge-files-with-ancestor path-a path-b ancestor-path nil path-c))
              (ediff-files3 path-a path-b path-c)))
          `((status . "success")
            (message . ,(if ancestor "Opened merge session" "Opened ediff3 session"))))
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
          (let ((full-path (expand-file-name file (projectile-project-root))))
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
                               (expand-file-name file (projectile-project-root))
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

(defun claude-code-emacs-mcp-handle-applyPatch (params)
  "Handle applyPatch request with PARAMS."
  (let ((patch-file (cdr (assoc 'patchFile params)))
        (target-file (cdr (assoc 'targetFile params))))
    (condition-case err
        (progn
          (unless (and patch-file target-file)
            (error "Missing required parameters: patchFile and targetFile"))
          (let ((patch-path (expand-file-name patch-file (projectile-project-root)))
                (target-path (expand-file-name target-file (projectile-project-root))))
            (unless (file-exists-p patch-path)
              (error "Patch file not found: %s" patch-path))
            (unless (file-exists-p target-path)
              (error "Target file not found: %s" target-path))
            (ediff-patch-file patch-path target-path))
          `((status . "success")
            (message . "Patch session started")))
      (error
       `((status . "error")
         (message . ,(error-message-string err)))))))

;;; Resource Handlers

(defun claude-code-emacs-mcp-handle-get-buffer-content (params)
  "Get content of a buffer specified by PATH in PARAMS."
  (let* ((path (cdr (assoc 'path params)))
         (full-path (expand-file-name path (projectile-project-root))))
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
  (let ((project-root (projectile-project-root)))
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
                            (absolutePath . ,(expand-file-name file (projectile-project-root)))))
                        files)))))

;;; Command execution

(defun claude-code-emacs-mcp-handle-runCommand (params)
  "Handle runCommand request with PARAMS."
  (let* ((command-name (cdr (assoc 'command params)))
         (args (cdr (assoc 'args params)))
         (interactive-p (cdr (assoc 'interactive params)))
         (current-buffer-p (cdr (assoc 'currentBuffer params)))
         (command-sym (intern-soft command-name)))

    (condition-case err
        (progn
          ;; Validate command exists
          (unless command-sym
            (error "Unknown command: %s" command-name))

          ;; Check if command is actually a command
          (unless (commandp command-sym)
            (error "Not a command: %s" command-name))

          ;; Additional security check on Emacs side
          (when (claude-code-emacs-mcp-command-blocked-p command-name)
            (error "Command '%s' is blocked for security reasons" command-name))

          ;; Execute the command
          (let* ((original-buffer (current-buffer))
                 (original-point (point))
                 (output-buffer (generate-new-buffer " *MCP Command Output*"))
                 (result nil)
                 (buffer-changed nil))

            (unwind-protect
                (progn
                  ;; Capture any output
                  (let ((standard-output output-buffer))

                    ;; Execute in appropriate context
                    (if current-buffer-p
                        ;; Stay in current buffer
                        (progn
                          (setq result
                                (if interactive-p
                                    (call-interactively command-sym)
                                  (apply command-sym args)))
                          (setq buffer-changed (buffer-modified-p)))

                      ;; Execute without buffer context
                      (with-temp-buffer
                        (setq result
                              (if interactive-p
                                  (call-interactively command-sym)
                                (apply command-sym args))))))

                  ;; Return success response
                  `((success . t)
                    (result . ,(claude-code-emacs-mcp-serialize-value result))
                    (output . ,(with-current-buffer output-buffer
                                 (buffer-string)))
                    (bufferChanged . ,buffer-changed)))

              ;; Cleanup
              (kill-buffer output-buffer))))

      (error
       `((success . nil)
         (error . ,(error-message-string err)))))))

(defun claude-code-emacs-mcp-command-blocked-p (command-name)
  "Check if COMMAND-NAME should be blocked for security."
  (member command-name
          '(;; System/Shell commands
            "shell-command"
            "async-shell-command"
            "compile"
            "shell"
            "term"
            "eshell"
            "vterm"

            ;; File system destructive
            "delete-file"
            "delete-directory"
            "move-file-to-trash"

            ;; Emacs exit
            "save-buffers-kill-emacs"
            "kill-emacs"

            ;; Package management
            "package-install"
            "package-delete"
            "package-refresh-contents"

            ;; Code evaluation
            "eval-expression"
            "eval-region"
            "eval-buffer"
            "eval-last-sexp"
            "eval-defun"
            "eval-print-last-sexp"

            ;; Loading files
            "load-file"
            "load-library"
            "require"

            ;; Network
            "url-retrieve"
            "browse-url"
            "eww")))

(defun claude-code-emacs-mcp-serialize-value (value)
  "Serialize VALUE for JSON encoding."
  (cond
   ;; nil -> null
   ((null value) :null)
   ;; Simple types
   ((numberp value) value)
   ((stringp value) value)
   ((booleanp value) value)
   ;; Symbol -> string
   ((symbolp value) (symbol-name value))
   ;; List -> array
   ((listp value)
    (mapcar #'claude-code-emacs-mcp-serialize-value value))
   ;; Vector -> array
   ((vectorp value)
    (append (mapcar #'claude-code-emacs-mcp-serialize-value value) nil))
   ;; Buffer -> buffer info
   ((bufferp value)
    `((type . "buffer")
      (name . ,(buffer-name value))
      (file . ,(buffer-file-name value))))
   ;; Marker -> position info
   ((markerp value)
    `((type . "marker")
      (position . ,(marker-position value))
      (buffer . ,(when (marker-buffer value)
                   (buffer-name (marker-buffer value))))))
   ;; Default: convert to string
   (t (format "%S" value))))

;;; Definition finding

(defun claude-code-emacs-mcp-handle-getDefinition (params)
  "Handle getDefinition request with PARAMS."
  (let* ((symbol-name (cdr (assoc 'symbol params)))
         (file-path (cdr (assoc 'file params)))
         (line (cdr (assoc 'line params)))
         (column (cdr (assoc 'column params)))
         (definitions '())
         (method nil)
         (searched-symbol nil))
    
    (condition-case err
        (progn
          ;; If file path is provided, visit that file first
          (when file-path
            (let* ((full-path (expand-file-name file-path (projectile-project-root)))
                   (buffer (find-file-noselect full-path)))
              (with-current-buffer buffer
                (when (and line column)
                  (goto-char (point-min))
                  (forward-line (1- line))
                  (move-to-column column))
                
                ;; Try to get symbol at point if not provided
                (unless symbol-name
                  (setq symbol-name (thing-at-point 'symbol t))))))
          
          ;; If symbol provided without file context, use current buffer
          (when (and symbol-name (not file-path))
            (setq searched-symbol symbol-name))
          
          ;; Try LSP first if available
          (when (and (fboundp 'lsp-mode)
                     (bound-and-true-p lsp-mode)
                     (fboundp 'lsp-find-definition))
            (condition-case nil
                (let ((lsp-defs (claude-code-emacs-mcp-get-lsp-definitions)))
                  (when lsp-defs
                    (setq definitions lsp-defs)
                    (setq method "lsp")))
              (error nil)))
          
          ;; Fall back to xref if no LSP results
          (when (and (null definitions)
                     (fboundp 'xref-find-definitions))
            (condition-case nil
                (let ((xref-defs (claude-code-emacs-mcp-get-xref-definitions 
                                  (or symbol-name (thing-at-point 'symbol t)))))
                  (when xref-defs
                    (setq definitions xref-defs)
                    (setq method "xref")))
              (error nil)))
          
          ;; Return results
          (if definitions
              `((definitions . ,definitions)
                (searchedSymbol . ,(or searched-symbol 
                                        symbol-name 
                                        (thing-at-point 'symbol t)))
                (method . ,method))
            (error "No definition found")))
      
      (error
       (error "Failed to find definition: %s" (error-message-string err))))))

(defun claude-code-emacs-mcp-get-lsp-definitions ()
  "Get definitions using LSP."
  (let ((current-point (point))
        (definitions '()))
    ;; lsp-find-definition moves point, so we need to capture the results
    (save-excursion
      (call-interactively 'lsp-find-definition)
      ;; Check if we moved to a different location
      (unless (= (point) current-point)
        (push (claude-code-emacs-mcp-capture-definition-at-point) definitions)))
    definitions))

(defun claude-code-emacs-mcp-get-xref-definitions (symbol)
  "Get definitions using xref for SYMBOL."
  (when symbol
    (let* ((xrefs (xref-backend-definitions (xref-find-backend) symbol))
           (definitions '()))
      (dolist (xref xrefs)
        (let* ((location (xref-item-location xref))
               (file (xref-location-group location))
               (position (xref-location-marker location)))
          (when (and file position)
            (with-current-buffer (find-file-noselect file)
              (save-excursion
                (goto-char position)
                (push (claude-code-emacs-mcp-capture-definition-at-point) definitions))))))
      (nreverse definitions))))

(defun claude-code-emacs-mcp-capture-definition-at-point ()
  "Capture definition information at current point."
  (let* ((file (buffer-file-name))
         (line (line-number-at-pos))
         (column (current-column))
         (symbol (thing-at-point 'symbol t))
         (preview (claude-code-emacs-mcp-get-definition-preview)))
    `((file . ,file)
      (line . ,line)
      (column . ,column)
      (symbol . ,symbol)
      (type . ,(claude-code-emacs-mcp-guess-definition-type))
      (preview . ,preview))))

(defun claude-code-emacs-mcp-get-definition-preview ()
  "Get preview text around current definition."
  (save-excursion
    (let ((start (progn (beginning-of-defun) (point)))
          (end (progn (end-of-defun) (point))))
      ;; Limit preview to reasonable size
      (when (> (- end start) 500)
        (setq end (+ start 500)))
      (buffer-substring-no-properties start end))))

(defun claude-code-emacs-mcp-guess-definition-type ()
  "Guess the type of definition at point."
  (save-excursion
    (beginning-of-defun)
    (cond
     ;; Emacs Lisp
     ((looking-at "(defun\\s-") "function")
     ((looking-at "(defvar\\s-\\|(defcustom\\s-") "variable")
     ((looking-at "(defclass\\s-") "class")
     ((looking-at "(defmethod\\s-") "method")
     ((looking-at "(defmacro\\s-") "macro")
     ((looking-at "(defconst\\s-") "constant")
     ;; Generic patterns for other languages
     ((looking-at "\\(function\\|def\\)\\s-") "function")
     ((looking-at "\\(class\\)\\s-") "class")
     ((looking-at "\\(const\\|let\\|var\\)\\s-") "variable")
     (t "definition"))))

(provide 'claude-code-emacs-mcp-tools)
;;; claude-code-emacs-mcp-tools.el ends here
