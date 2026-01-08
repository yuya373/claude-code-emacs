;;; test-claude-code-edit.el --- Tests for claude-code-edit.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: George Mauer
;; Keywords: tools, convenience

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

;; Tests for claude-code-edit.el

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'claude-code-edit)

;;; Test utilities

(defun test-claude-code-edit--cleanup ()
  "Clean up test buffers."
  (when-let ((buf (get-buffer "*claude-code-edit:/test/project*")))
    (kill-buffer buf))
  (when-let ((buf (get-buffer "*claude:/test/project*")))
    (kill-buffer buf)))

;;; Tests for buffer management

(ert-deftest test-claude-code-edit-buffer-name ()
  "Test edit buffer naming."
  (let ((projectile-project-root "/test/project/"))
    (should (equal (claude-code-edit-buffer-name)
                   "*claude-code-edit:/test/project*"))))

(ert-deftest test-claude-code-edit-buffer-name-nil-when-no-project ()
  "Test edit buffer name returns nil when not in a project."
  (let ((projectile-project-root nil))
    (should (null (claude-code-edit-buffer-name)))))

(ert-deftest test-claude-code-edit-get-buffer ()
  "Test getting edit buffer."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/"))
    ;; Buffer doesn't exist yet
    (should (null (claude-code-edit-get-buffer)))

    ;; Create buffer
    (get-buffer-create "*claude-code-edit:/test/project*")

    ;; Now it should exist
    (should (bufferp (claude-code-edit-get-buffer)))

    (test-claude-code-edit--cleanup)))

(ert-deftest test-claude-code-edit-get-or-create-buffer ()
  "Test getting or creating edit buffer."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/"))
    ;; Buffer doesn't exist, should create it
    (let ((buf (claude-code-edit-get-or-create-buffer)))
      (should (bufferp buf))
      (should (equal (buffer-name buf) "*claude-code-edit:/test/project*")))

    ;; Second call should return same buffer
    (let ((buf1 (claude-code-edit-get-or-create-buffer))
          (buf2 (claude-code-edit-get-or-create-buffer)))
      (should (eq buf1 buf2)))

    (test-claude-code-edit--cleanup)))

;;; Tests for mode

(ert-deftest test-claude-code-edit-mode-keymap ()
  "Test edit mode has correct keybindings."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/"))
    (with-current-buffer (claude-code-edit-get-or-create-buffer)
      (claude-code-edit-mode 1)
      ;; Check keybindings exist
      (should (keymapp claude-code-edit-mode-map))
      (should (eq (lookup-key claude-code-edit-mode-map (kbd "C-c C-c"))
                  'claude-code-edit-finish))
      (should (eq (lookup-key claude-code-edit-mode-map (kbd "C-c C-k"))
                  'claude-code-edit-cancel)))
    (test-claude-code-edit--cleanup)))

;;; Tests for main commands

(ert-deftest test-claude-code-edit-special-requires-session ()
  "Test edit special requires an active Claude Code session."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/"))
    ;; Mock ensure-buffer to simulate no session
    (cl-letf (((symbol-function 'claude-code-ensure-buffer)
               (lambda () (error "No Claude Code session for this project"))))
      (should-error (claude-code-edit-special)
                    :type 'error))
    (test-claude-code-edit--cleanup)))

(ert-deftest test-claude-code-edit-finish-sends-content ()
  "Test edit finish sends buffer content."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/")
        (sent-string nil))
    ;; Mock send-string to capture what's sent
    (cl-letf (((symbol-function 'claude-code-send-string)
               (lambda (string &optional _paste-p)
                 (setq sent-string string))))

      (with-current-buffer (claude-code-edit-get-or-create-buffer)
        (claude-code-edit-mode 1)
        (erase-buffer)
        (insert "Test message\n\nWith multiple lines")

        ;; Mock window management
        (cl-letf (((symbol-function 'get-buffer-window)
                   (lambda (_buf) nil))
                  ((symbol-function 'quit-window)
                   (lambda (&rest _args) nil)))
          (claude-code-edit-finish)

          ;; Check that content was sent (trimmed)
          (should (equal sent-string "Test message\n\nWith multiple lines"))

          ;; Buffer should be cleared
          (should (string-empty-p (string-trim (buffer-string)))))))

    (test-claude-code-edit--cleanup)))

(ert-deftest test-claude-code-edit-finish-empty-buffer ()
  "Test edit finish with empty buffer."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/")
        (sent-string nil))
    ;; Mock send-string to capture what's sent
    (cl-letf (((symbol-function 'claude-code-send-string)
               (lambda (string &optional _paste-p)
                 (setq sent-string string))))

      (with-current-buffer (claude-code-edit-get-or-create-buffer)
        (claude-code-edit-mode 1)
        (erase-buffer)

        (claude-code-edit-finish)

        ;; Nothing should be sent for empty buffer
        (should (null sent-string))))

    (test-claude-code-edit--cleanup)))

(ert-deftest test-claude-code-edit-cancel-preserves-content ()
  "Test edit cancel preserves buffer content."
  (test-claude-code-edit--cleanup)
  (let ((projectile-project-root "/test/project/"))
    (with-current-buffer (claude-code-edit-get-or-create-buffer)
      (claude-code-edit-mode 1)
      (erase-buffer)
      (insert "Test content that should be preserved")

      ;; Mock window management
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (_buf) nil))
                ((symbol-function 'quit-window)
                 (lambda (&rest _args) nil)))

        (claude-code-edit-cancel)

        ;; Content should still be there
        (should (equal (buffer-string)
                       "Test content that should be preserved"))))

    (test-claude-code-edit--cleanup)))

;;; Test customization

(ert-deftest test-claude-code-edit-buffer-window-setup-valid-values ()
  "Test that customization accepts valid values."
  (dolist (value '(current-window other-window reorganize-frame other-frame))
    (setq claude-code-edit-buffer-window-setup value)
    (should (eq claude-code-edit-buffer-window-setup value))))

(provide 'test-claude-code-edit)
;;; test-claude-code-edit.el ends here
