;;; test-claude-code-emacs-buffer.el --- Tests for buffer management and key sending -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Claude Code
;; Keywords: test

;;; Commentary:

;; Test suite for claude-code-emacs-buffer module

;;; Code:

(require 'ert)
(require 'claude-code-emacs-buffer)
(require 'cl-lib)

;;; Test utilities

(defmacro with-claude-test-project (&rest body)
  "Execute BODY in a temporary project directory."
  `(let* ((temp-dir (make-temp-file "claude-test-" t))
          (default-directory temp-dir)
          (projectile-project-root temp-dir))
     (unwind-protect
         (progn
           ;; Create a minimal .git directory to make projectile recognize it as a project
           (make-directory (expand-file-name ".git" temp-dir))
           ,@body)
       (delete-directory temp-dir t))))

(defmacro with-claude-mock-buffer (&rest body)
  "Execute BODY with a mock Claude Code buffer."
  `(with-claude-test-project
    (let ((buffer-name (claude-code-emacs-buffer-name)))
      (with-current-buffer (get-buffer-create buffer-name)
        (claude-code-emacs-vterm-mode)
        ,@body)
      (when (get-buffer buffer-name)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buffer-name))))))

;;; Tests for buffer management

(ert-deftest test-claude-code-emacs-buffer-name ()
  "Test buffer name generation."
  (with-claude-test-project
   (should (string-match-p "\\*claude:.*\\*" (claude-code-emacs-buffer-name)))
   (should (string-match-p (regexp-quote temp-dir) (claude-code-emacs-buffer-name)))))

(ert-deftest test-claude-code-emacs-switch-to-buffer ()
  "Test switching to Claude Code buffer."
  (with-claude-mock-buffer
   (let ((buffer-name (claude-code-emacs-buffer-name)))
     ;; Test switching to existing buffer
     (with-temp-buffer
       (claude-code-emacs-switch-to-buffer)
       (should (string= (buffer-name) buffer-name)))

     ;; Test when buffer doesn't exist
     (let ((kill-buffer-query-functions nil))
       (kill-buffer buffer-name))
     (with-temp-buffer
       ;; Mock y-or-n-p to return nil (user says no)
       (cl-letf (((symbol-function 'y-or-n-p)
                  (lambda (_prompt) nil)))
         (let ((inhibit-message t))
           (claude-code-emacs-switch-to-buffer))
         ;; Should stay in current buffer when user declines
         (should-not (string= (buffer-name) buffer-name)))))))

;;; Tests for send functions with mock vterm

(ert-deftest test-claude-code-emacs-send-string-mock ()
  "Test send-string with mocked vterm functions."
  (cl-letf* ((vterm-strings nil)
             ((symbol-function 'vterm-send-string)
              (lambda (str &optional paste-p)
                (push str vterm-strings))))

    (with-claude-mock-buffer
     ;; Test normal send
     (claude-code-emacs-send-string "test command")
     (should (member "test command" vterm-strings)))))

(ert-deftest test-claude-code-emacs-special-key-functions ()
  "Test special key sending functions."
  (cl-letf* ((escape-sent nil)
             (return-sent nil)
             (ctrl-r-sent nil)
             ((symbol-function 'vterm-send-escape)
              (lambda () (setq escape-sent t)))
             ((symbol-function 'vterm-send-return)
              (lambda () (setq return-sent t)))
             ((symbol-function 'vterm-send-key)
              (lambda (key &optional shift)
                ;; Check if the key is Ctrl-R (character code 18)
                (when (and (stringp key) (= (aref key 0) ?\C-r))
                  (setq ctrl-r-sent t))))
             ((symbol-function 'kbd)
              (lambda (key-string)
                (cond ((string= key-string "C-r") "\C-r")
                      ((string= key-string "<backtab>") "<backtab>")
                      (t key-string)))))

    (with-claude-mock-buffer
     ;; Test escape sending
     (claude-code-emacs-send-escape)
     (should escape-sent)

     ;; Test return sending
     (claude-code-emacs-send-return)
     (should return-sent)

     ;; Test ctrl-r sending
     (claude-code-emacs-send-ctrl-r)
     (should ctrl-r-sent))))

;;; Tests for quick send functions

(ert-deftest test-claude-code-emacs-quick-send-functions ()
  "Test quick send functions."
  (cl-letf* ((sent-commands nil)
             ((symbol-function 'claude-code-emacs-send-string)
              (lambda (str &optional paste-p)
                (push str sent-commands))))

    ;; Test number sending functions
    (claude-code-emacs-send-1)
    (should (member "1" sent-commands))

    (claude-code-emacs-send-2)
    (should (member "2" sent-commands))

    (claude-code-emacs-send-3)
    (should (member "3" sent-commands))

    ;; Test commit sending
    (claude-code-emacs-send-commit)
    (should (member "commit" sent-commands))))

;;; Tests for buffer path functions

(ert-deftest test-claude-code-emacs-format-buffer-path ()
  "Test formatting buffer path with @ prefix."
  (with-claude-test-project
   ;; Test file in project
   (let ((test-file (expand-file-name "test.el" temp-dir)))
     (with-temp-file test-file
       (insert "test content"))
     (with-current-buffer (find-file-noselect test-file)
       (let ((result (claude-code-emacs-format-buffer-path (current-buffer))))
         (should (equal result "@test.el")))
       (kill-buffer)))

   ;; Test file in subdirectory
   (let ((sub-dir (expand-file-name "src" temp-dir)))
     (make-directory sub-dir)
     (let ((test-file (expand-file-name "main.el" sub-dir)))
       (with-temp-file test-file
         (insert "test content"))
       (with-current-buffer (find-file-noselect test-file)
         (let ((result (claude-code-emacs-format-buffer-path (current-buffer))))
           (should (equal result "@src/main.el")))
         (kill-buffer))))

   ;; Test buffer without file
   (with-temp-buffer
     (should-not (claude-code-emacs-format-buffer-path (current-buffer))))

   ;; Test file outside project
   (let ((outside-file "/tmp/test-outside.el"))
     (with-temp-file outside-file
       (insert "test content"))
     (with-current-buffer (find-file-noselect outside-file)
       (should-not (claude-code-emacs-format-buffer-path (current-buffer)))
       (kill-buffer))
     (delete-file outside-file))))

(ert-deftest test-claude-code-emacs-get-buffer-paths ()
  "Test getting all buffer paths with @ prefix."
  (with-claude-test-project
   ;; Create test files
   (let ((file1 (expand-file-name "file1.el" temp-dir))
         (file2 (expand-file-name "file2.el" temp-dir))
         (sub-dir (expand-file-name "lib" temp-dir)))
     (make-directory sub-dir)
     (let ((file3 (expand-file-name "util.el" sub-dir)))

       ;; Create files
       (with-temp-file file1 (insert "content1"))
       (with-temp-file file2 (insert "content2"))
       (with-temp-file file3 (insert "content3"))

       ;; Open files in buffers
       (let ((buf1 (find-file-noselect file1))
             (buf2 (find-file-noselect file2))
             (buf3 (find-file-noselect file3)))

         ;; Test get-buffer-paths
         (let ((paths (claude-code-emacs-get-buffer-paths)))
           (should (member "@file1.el" paths))
           (should (member "@file2.el" paths))
           (should (member "@lib/util.el" paths)))

         ;; Clean up
         (kill-buffer buf1)
         (kill-buffer buf2)
         (kill-buffer buf3))))))

;;; Tests for file path insertion functions

(ert-deftest test-claude-code-emacs-insert-file-path ()
  "Test inserting individual file paths."
  (cl-letf* ((selected-path nil)
             ((symbol-function 'completing-read)
              (lambda (prompt choices &rest args)
                (setq selected-path (car choices))
                selected-path)))

    (with-claude-test-project
     ;; Create test files
     (let ((file1 (expand-file-name "test.el" temp-dir)))
       (with-temp-file file1 (insert "content"))
       (let ((buf1 (find-file-noselect file1)))

         ;; Test in a temp buffer
         (with-temp-buffer
           (claude-code-emacs-insert-file-path)
           (should (equal (buffer-string) "@test.el")))

         ;; Clean up
         (kill-buffer buf1))))))

(ert-deftest test-claude-code-emacs-insert-open-buffer-paths ()
  "Test inserting open buffer file paths."
  (with-claude-test-project
   ;; Create test files
   (let ((file1 (expand-file-name "file1.el" temp-dir))
         (file2 (expand-file-name "file2.el" temp-dir)))
     (with-temp-file file1 (insert "content1"))
     (with-temp-file file2 (insert "content2"))

     (let ((buf1 (find-file-noselect file1))
           (buf2 (find-file-noselect file2)))

       ;; Test in a temp buffer
       (with-temp-buffer
         (claude-code-emacs-insert-open-buffer-paths)
         (let ((content (buffer-string)))
           (should (string-match-p "@file1\\.el" content))
           (should (string-match-p "@file2\\.el" content))
           (should (string-match-p "\n" content))))

       ;; Clean up
       (kill-buffer buf1)
       (kill-buffer buf2)))))

(ert-deftest test-claude-code-emacs-at-sign-complete ()
  "Test @ completion functionality."
  (cl-letf* ((selected-file nil)
             ((symbol-function 'completing-read)
              (lambda (prompt choices &rest args)
                (setq selected-file (car choices))
                selected-file))
             ((symbol-function 'projectile-project-files)
              (lambda (root)
                '("test.el" "src/main.el"))))

    (with-claude-test-project
     ;; Test @ completion
     (with-temp-buffer
       (claude-code-emacs-at-sign-complete)
       (should (equal (buffer-string) "@test.el"))))))

(provide 'test-claude-code-emacs-buffer)
;;; test-claude-code-emacs-buffer.el ends here
