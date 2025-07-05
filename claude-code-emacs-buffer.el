;;; claude-code-emacs-buffer.el --- Buffer management for Claude Code Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yuya Minami
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

;; Buffer management and key sending functionality for Claude Code Emacs.

;;; Code:

(require 'vterm)
(require 'claude-code-emacs-core)

(defvar claude-code-emacs-executable)   ;; Defined in core

;;; Buffer Operations

(defun claude-code-emacs-send-string (string &optional paste-p)
  "Send STRING to the Claude Code session."
  (interactive "sEnter text: ")
  (claude-code-emacs-with-vterm-buffer
   (lambda ()
     (vterm-send-string string paste-p)
     ;; NOTE: wait for `accept-process-output' in `vterm-send-string'
     (sit-for (* vterm-timer-delay 3))
     (vterm-send-return))))

(provide 'claude-code-emacs-buffer)
;;; claude-code-emacs-buffer.el ends here
