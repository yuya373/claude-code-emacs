;;; test-claude-code-mcp-events.el --- Tests for MCP event system -*- lexical-binding: t; -*-

;;; Commentary:
;; Test suite for claude-code-mcp-events.el

;;; Code:

(require 'ert)

(require 'claude-code-mcp-events)
(require 'claude-code-mcp)
(require 'claude-code-mcp-connection)

;; Test helper for tracking sent messages
(defvar test-mcp-sent-messages nil)

(defun test-mcp-capture-send-event-to-project (project-root event-name params)
  "Capture messages sent via mcp send event to project."
  (push (list :project project-root :event event-name :params params) test-mcp-sent-messages))

(defmacro with-mcp-message-capture (&rest body)
  "Execute BODY while capturing MCP messages."
  `(let ((test-mcp-sent-messages nil))
     (cl-letf (((symbol-function 'claude-code-mcp-send-event-to-project)
                #'test-mcp-capture-send-event-to-project))
       ,@body)))

(defmacro with-test-buffer (name content &rest body)
  "Create a test buffer with NAME and CONTENT, execute BODY."
  (declare (indent 2))
  `(let ((buf (get-buffer-create ,name)))
     (unwind-protect
         (with-current-buffer buf
           (erase-buffer)
           (insert ,content)
           (setq buffer-file-name ,name)
           ,@body)
       (kill-buffer buf))))

(defmacro with-mock-project-root (root &rest body)
  "Execute BODY with mocked project root."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'projectile-project-root)
              (lambda () ,root)))
     ,@body))

;; Buffer list update tests
(ert-deftest test-mcp-buffer-list-event ()
  "Test buffer list update event generation."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      ;; Mock project connections
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal)))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        (with-test-buffer "/test/project/file1.el" "content1"
          (with-test-buffer "/test/project/file2.el" "content2"
            (claude-code-mcp-events-send-buffer-list-update)

            ;; Should send one notification
            (should (= 1 (length test-mcp-sent-messages)))

            (let* ((msg (car test-mcp-sent-messages))
                   (buffers (cdr (assoc 'buffers (plist-get msg :params)))))
              (should (equal (plist-get msg :event) "bufferListUpdated"))
              (should (equal (plist-get msg :project) "/test/project"))
              (should (= 2 (length buffers)))
              (let ((file-paths (mapcar (lambda (b) (cdr (assoc 'path b))) buffers)))
                (should (member "/test/project/file1.el" file-paths))
                (should (member "/test/project/file2.el" file-paths))))))))))

(ert-deftest test-mcp-buffer-list-filtering ()
  "Test that buffer list is filtered by project."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      ;; Mock project connections
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal)))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        (with-test-buffer "/test/project/file1.el" "content1"
          (with-test-buffer "/other/project/file2.el" "content2"
            (claude-code-mcp-events-send-buffer-list-update)

            (let* ((msg (car test-mcp-sent-messages))
                   (buffers (cdr (assoc 'buffers (plist-get msg :params)))))
              ;; Should only include buffer from the target project
              (should (= 1 (length buffers)))
              (let ((file-paths (mapcar (lambda (b) (cdr (assoc 'path b))) buffers)))
                (should (equal "/test/project/file1.el" (car file-paths))))))))))

;; Buffer content change tests
(ert-deftest test-mcp-content-change-event ()
  "Test buffer content change event."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal)))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        (with-test-buffer "/test/project/file.el" "line1\nline2\nline3"
          ;; Track a change
          (claude-code-mcp-events-after-change 7 13 0)

          ;; Trigger batch send
          (claude-code-mcp-events-send-buffer-changes)

          ;; Should send one notification
          (should (= 1 (length test-mcp-sent-messages)))

          (let* ((msg (car test-mcp-sent-messages))
                 (changes (cdr (assoc 'changes (plist-get msg :params)))))
            (should (equal (plist-get msg :event) "bufferContentModified"))
            (should (equal (plist-get msg :project) "/test/project"))
            (should (= 1 (length changes)))

            (let ((change (car changes)))
              (should (equal (cdr (assoc 'file change)) "/test/project/file.el"))
              (should (= (cdr (assoc 'startLine change)) 2))
              (should (= (cdr (assoc 'endLine change)) 2))
              (should (= (cdr (assoc 'changeLength change)) 0)))))))))

(ert-deftest test-mcp-content-change-batching ()
  "Test that multiple changes are batched together."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal))
            (claude-code-mcp-events-pending-changes nil))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        (with-test-buffer "/test/project/file1.el" "content"
          (with-test-buffer "/test/project/file2.el" "content"
            ;; Manually add changes to pending list
            (push (list "/test/project/file1.el" 1 1 10 "/test/project")
                  claude-code-mcp-events-pending-changes)
            (push (list "/test/project/file2.el" 5 6 20 "/test/project")
                  claude-code-mcp-events-pending-changes)

            ;; Trigger batch send
            (claude-code-mcp-events-send-buffer-changes)

            ;; Should send one notification with both changes
            (should (= 1 (length test-mcp-sent-messages)))

            (let* ((msg (car test-mcp-sent-messages))
                   (changes (cdr (assoc 'changes (plist-get msg :params)))))
              (should (= 2 (length changes)))

              ;; Verify both changes are included
              (let ((files (mapcar (lambda (c) (cdr (assoc 'file c))) changes)))
                (should (member "/test/project/file1.el" files))
                (should (member "/test/project/file2.el" files)))))))))))

(ert-deftest test-mcp-content-change-merge ()
  "Test that overlapping changes are merged."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal))
            (claude-code-mcp-events-pending-changes nil))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        (with-test-buffer "/test/project/file.el" "line1\nline2\nline3\nline4\nline5\n"
          ;; Simulate after-change calls that update the same change
          (claude-code-mcp-events-after-change 7 13 0)  ;; Line 2-3
          ;; This should merge with the existing change
          (claude-code-mcp-events-after-change 14 20 0) ;; Line 3-4

          ;; Trigger batch send
          (claude-code-mcp-events-send-buffer-changes)

          ;; Should send one merged change
          (let* ((msg (car test-mcp-sent-messages))
                 (changes (cdr (assoc 'changes (plist-get msg :params)))))
            (should (= 1 (length changes)))

            (let ((change (car changes)))
              ;; Should merge to cover lines 2-4
              (should (= (cdr (assoc 'startLine change)) 2))
              (should (= (cdr (assoc 'endLine change)) 4)))))))

;; Diagnostics change tests
(ert-deftest test-mcp-diagnostics-event ()
  "Test diagnostics change event."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal)))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        ;; Mock LSP diagnostics
        (cl-letf (((symbol-function 'fboundp)
                   (lambda (sym)
                     (or (eq sym 'lsp-diagnostics)
                         (eq sym 'lsp:diagnostic-message)
                         (eq sym 'lsp:position-character)
                         (eq sym 'lsp:range-start)
                         (eq sym 'lsp:diagnostic-range)
                         (eq sym 'lsp:diagnostic-severity))))
                  ((symbol-function 'lsp-diagnostics)
                   (lambda ()
                     (let ((diags (make-hash-table :test 'equal)))
                       (puthash "/test/project/file.el"
                                (let ((line-diags (make-hash-table :test 'equal)))
                                  (puthash 1 (list (list :message "Test error"
                                                         :severity 1
                                                         :range (list :start (list :line 0 :character 0)
                                                                      :end (list :line 0 :character 5))))
                                           line-diags)
                                  line-diags)
                                diags)
                       diags)))
                  ((symbol-function 'lsp:diagnostic-message)
                   (lambda (diag) (plist-get diag :message)))
                  ((symbol-function 'lsp:diagnostic-severity)
                   (lambda (diag) (plist-get diag :severity)))
                  ((symbol-function 'lsp:diagnostic-range)
                   (lambda (diag) (plist-get diag :range)))
                  ((symbol-function 'lsp:range-start)
                   (lambda (range) (plist-get range :start)))
                  ((symbol-function 'lsp:position-character)
                   (lambda (pos) (plist-get pos :character)))
                  ((symbol-function 'find-buffer-visiting)
                   (lambda (file) (get-buffer-create file))))

          ;; Trigger diagnostics send
          (claude-code-mcp-events-send-diagnostics-update)

          ;; Should send one notification
          (should (= 1 (length test-mcp-sent-messages)))

          (let* ((msg (car test-mcp-sent-messages))
                 (files (cdr (assoc 'files (plist-get msg :params)))))
            (should (equal (plist-get msg :event) "diagnosticsChanged"))
            (should (equal (plist-get msg :project) "/test/project"))
            (should (= 1 (length files)))

            (let* ((file-diag (car files))
                   (diags (cdr (assoc 'diagnostics file-diag))))
              (should (equal (cdr (assoc 'file file-diag)) "/test/project/file.el"))
              (should (= 1 (length diags)))
              (should (equal (cdr (assoc 'message (car diags))) "Test error")))))))))

(ert-deftest test-mcp-diagnostics-batching ()
  "Test that diagnostics for multiple projects are sent separately."
  (with-mcp-message-capture
    (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal)))
      (puthash "/project1" t claude-code-mcp-project-connections)
      (puthash "/project2" t claude-code-mcp-project-connections)

      ;; Mock LSP diagnostics for multiple files
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym)
                   (or (eq sym 'lsp-diagnostics)
                       (eq sym 'lsp:diagnostic-message))))
                ((symbol-function 'lsp-diagnostics)
                 (lambda ()
                   (let ((diags (make-hash-table :test 'equal)))
                     (puthash "/project1/file.el"
                              (let ((line-diags (make-hash-table :test 'equal)))
                                (puthash 1 (list (list :message "Error in project1"))
                                         line-diags)
                                line-diags)
                              diags)
                     (puthash "/project2/file.el"
                              (let ((line-diags (make-hash-table :test 'equal)))
                                (puthash 1 (list (list :message "Error in project2"))
                                         line-diags)
                                line-diags)
                              diags)
                     diags)))
                ((symbol-function 'lsp:diagnostic-message)
                 (lambda (diag) (plist-get diag :message)))
                ((symbol-function 'find-buffer-visiting)
                 (lambda (file)
                   (with-current-buffer (get-buffer-create file)
                     (setq buffer-file-name file)
                     (current-buffer))))
                ((symbol-function 'projectile-project-root)
                 (lambda ()
                   (cond ((string-match "^/project1/" buffer-file-name) "/project1")
                         ((string-match "^/project2/" buffer-file-name) "/project2")
                         (t nil)))))

        ;; Trigger batch send
        (claude-code-mcp-events-send-diagnostics-update)

        ;; Should send two notifications (one per project)
        (should (= 2 (length test-mcp-sent-messages)))

        ;; Verify each project gets its own notification
        (let ((projects (mapcar (lambda (msg)
                                  (plist-get msg :project))
                                test-mcp-sent-messages)))
          (should (member "/project1" projects))
          (should (member "/project2" projects))))))
    ;; Cleanup test buffers
    (dolist (buf '("/project1/file.el" "/project2/file.el"))
      (when (get-buffer buf)
        (kill-buffer buf)))))

;; Integration tests
(ert-deftest test-mcp-event-system-integration ()
  "Test the complete event system integration."
  (with-mcp-message-capture
    (with-mock-project-root "/test/project"
      (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal))
            (claude-code-mcp-events-pending-changes nil))
        (puthash "/test/project" t claude-code-mcp-project-connections)

        ;; Setup initial state
        (with-test-buffer "/test/project/file.el" "initial content"
          ;; Enable event tracking
          (claude-code-mcp-events-enable)

          ;; Simulate various events
          ;; 1. Buffer list update
          (claude-code-mcp-events-send-buffer-list-update)

          ;; 2. Content change
          (claude-code-mcp-events-after-change 1 15 0)
          (claude-code-mcp-events-send-buffer-changes)

          ;; 3. Mock diagnostics update
          (cl-letf (((symbol-function 'fboundp)
                     (lambda (sym) t))
                    ((symbol-function 'lsp-diagnostics)
                     (lambda ()
                       (let ((diags (make-hash-table :test 'equal)))
                         diags))))
            (claude-code-mcp-events-send-diagnostics-update))

          ;; Should have sent multiple notifications
          (should (>= (length test-mcp-sent-messages) 2))

          ;; Verify we got each type of notification
          (let ((events (mapcar (lambda (msg) (plist-get msg :event))
                                test-mcp-sent-messages)))
            (should (member "bufferListUpdated" events))
            (should (member "bufferContentModified" events)))

          ;; Disable events
          (claude-code-mcp-events-disable)))))))

(ert-deftest test-mcp-event-debouncing ()
  "Test that event debouncing works correctly."
  (let ((claude-code-mcp-events-change-delay 0.1)
        (claude-code-mcp-events-diagnostics-delay 0.1))
    (with-mcp-message-capture
      (with-mock-project-root "/test/project"
        (let ((claude-code-mcp-project-connections (make-hash-table :test 'equal))
              (claude-code-mcp-events-pending-changes nil))
          (puthash "/test/project" t claude-code-mcp-project-connections)

          (with-test-buffer "/test/project/file.el" "content"
            ;; Track multiple rapid changes
            (claude-code-mcp-events-after-change 1 2 0)
            (claude-code-mcp-events-after-change 3 4 0)
            (claude-code-mcp-events-after-change 5 6 0)

            ;; No messages should be sent immediately
            (should (= 0 (length test-mcp-sent-messages)))

            ;; Force send
            (claude-code-mcp-events-send-buffer-changes)

            ;; Should batch all changes into one message
            (should (= 1 (length test-mcp-sent-messages)))

            (let* ((msg (car test-mcp-sent-messages))
                   (changes (cdr (assoc 'changes (plist-get msg :params)))))
              ;; All changes should be in one notification
              (should (>= (length changes) 1)))))))))

(provide 'test-claude-code-mcp-events)
;;; test-claude-code-mcp-events.el ends here
