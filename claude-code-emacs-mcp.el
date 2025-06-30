;;; claude-code-emacs-mcp.el --- MCP server integration for Claude Code Emacs -*- lexical-binding: t; -*-

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

;; MCP (Model Context Protocol) server integration for Claude Code Emacs.
;; This module provides Emacs functionality access to Claude Code through MCP.
;;
;; Features:
;; - Per-project WebSocket connections
;; - File opening with selection
;; - Buffer listing
;; - Text selection retrieval
;; - LSP diagnostics integration

;;; Code:

;; Load MCP modules
(require 'claude-code-emacs-mcp-connection)
(require 'claude-code-emacs-mcp-protocol)
(require 'claude-code-emacs-mcp-tools)

(provide 'claude-code-emacs-mcp)
;;; claude-code-emacs-mcp.el ends here
