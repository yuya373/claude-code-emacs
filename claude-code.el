;;; claude-code.el --- Run Claude Code sessions -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Yuya Minami
;; Keywords: tools, convenience
;; Version: 0.5.0
;; URL: https://github.com/yuya373/claude-code-emacs
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
;; - MCP server integration with real-time event notifications
;;
;; Optional dependencies:
;; - lsp-mode (9.0.0): For LSP diagnostic fixing and MCP tools
;; - websocket (1.15): For MCP server WebSocket communication
;;
;; These are only required if you want to use MCP features or LSP diagnostics.
;;
;; Quick start:
;;
;;   (require 'claude-code)
;;   (global-set-key (kbd "C-c c") 'claude-code-transient)
;;
;; Basic usage:
;;
;;   M-x claude-code-run      ; Start Claude Code session
;;   C-c c                          ; Open transient menu
;;   C-u M-x claude-code-run  ; Start with options (model, resume, etc.)
;;
;; In prompt buffer (.claude-code.prompt.md):
;;   @ TAB                          ; Complete file paths
;;   C-c C-s                        ; Send section at point
;;   C-c C-b                        ; Send entire buffer
;;
;; MCP integration (optional):
;;
;;   M-x claude-code-install-mcp-server  ; Install MCP server
;;   ;; Then configure Claude Code as instructed
;;
;; For more information, see README.md

;;; Code:

(require 'vterm)

;; Load all modules
(require 'claude-code-core)
(require 'claude-code-commands)
(require 'claude-code-ui)
(require 'claude-code-prompt)

;; MCP integration (only when websocket is available)
(require 'claude-code-mcp)
(require 'claude-code-mcp-events)

(provide 'claude-code)
;;; claude-code.el ends here
