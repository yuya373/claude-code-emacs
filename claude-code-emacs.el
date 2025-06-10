;;; claude-code-emacs.el --- Run Claude Code sessions within Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yuya Minami
;; Keywords: tools, convenience
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (projectile "2.5.0") (vterm "0.0.2") (transient "0.4.0") (markdown-mode "2.5"))

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

;; This package provides integration with Claude Code CLI tool within Emacs.
;; It allows you to run Claude Code sessions in vterm buffers with project isolation.
;;
;; Main features:
;; - Project-specific Claude Code sessions
;; - Transient menu for common operations
;; - File path completion with @ syntax
;; - Custom commands support
;; - MCP server integration (optional)

;;; Code:

;; Load all modules
(require 'claude-code-emacs-core)
(require 'claude-code-emacs-buffer)
(require 'claude-code-emacs-commands)
(require 'claude-code-emacs-ui)
(require 'claude-code-emacs-prompt)

;; Optional MCP integration (only if dependencies are available)
(when (and (locate-library "claude-code-emacs-mcp")
           (ignore-errors (require 'websocket nil t)))
  (require 'claude-code-emacs-mcp nil t))

(provide 'claude-code-emacs)
;;; claude-code-emacs.el ends here
