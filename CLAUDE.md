# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an Emacs package that provides integration with Claude Code CLI. The package allows running Claude Code sessions within Emacs using vterm mode.

## Architecture

The main functionality is in `claude-code-emacs.el` which provides:

- `claude-code-emacs-run`: Main entry point that creates a Claude Code session buffer
- `claude-code-emacs-switch-to-buffer`: Switch to existing Claude Code buffer
- `claude-code-emacs-open-prompt-file`: Opens project-specific prompt file (`.claude-code-emacs.prompt.md`)
- Buffer management using projectile for project-aware naming 
- vterm-mode for terminal-like interaction
- Integration with Emacs workflow through interactive commands
- Transient menus for easy access to all functions

## Key Functions

### Session Management
- `claude-code-emacs-run`: Starts Claude Code CLI in a project-specific buffer
- `claude-code-emacs-switch-to-buffer`: Switch to existing Claude Code buffer for current project
- Buffer naming follows pattern `*claude:<project-root>*`

### Slash Commands
All slash commands have been implemented and use `claude-code-emacs-send-string` for consistency:
- `claude-code-emacs-init`: Sends `/init` command
- `claude-code-emacs-clear`: Sends `/clear` command
- `claude-code-emacs-help`: Sends `/help` command
- `claude-code-emacs-config`: Sends `/config` command with optional arguments
- `claude-code-emacs-memory`: Sends `/memory` command
- `claude-code-emacs-compact`: Sends `/compact` command with optional instructions
- `claude-code-emacs-cost`: Sends `/cost` command
- `claude-code-emacs-status`: Sends `/status` command
- `claude-code-emacs-review`: Sends `/review` command
- `claude-code-emacs-pr-comments`: Sends `/pr_comments` command
- `claude-code-emacs-bug`: Sends `/bug` command
- `claude-code-emacs-doctor`: Sends `/doctor` command
- `claude-code-emacs-login`: Sends `/login` command
- `claude-code-emacs-logout`: Sends `/logout` command

### Prompt Management
- `claude-code-emacs-open-prompt-file`: Creates/opens project-specific prompt file
- `claude-code-emacs-prompt-mode`: Markdown-based major mode for prompt files
- `claude-code-emacs-send-prompt-at-point`: Send markdown section at point to Claude Code
- `claude-code-emacs-send-prompt-region`: Send selected region to Claude Code
- LSP mode integration: Automatically configures language ID as "markdown" when lsp-mode is available

### Utility Functions
- `claude-code-emacs-send-string`: Core function for sending strings to Claude Code buffer
- `claude-code-emacs-send-region`: Send selected region or entire buffer
- `claude-code-emacs-chunk-string`: Splits long strings into manageable chunks

## Transient Menus

### Main Menu (`claude-code-emacs-transient`)
Organized into logical groups:
- Session: Run, switch buffer, open prompt file, send region
- Commands: Init, clear, help
- Memory & Config: Memory, config, compact
- Review: Review code, PR comments
- Info: Cost, status
- Account: Login, logout
- Other: Bug report, doctor

### Prompt Mode Menu (`claude-code-emacs-prompt-transient`)
Available in prompt buffers with `C-c C-t`:
- Send section at point
- Send region
- Run/switch to Claude Code

## Dependencies

- projectile: For project root detection
- vterm: Terminal emulation mode
- transient: Menu system
- markdown-mode: Base for prompt file editing
- lsp-mode (optional): For LSP integration

## Development Notes

This is a Japanese-language project (README in Japanese) that implements Emacs integration for Claude Code CLI. All slash commands have been implemented using a consistent interface through `claude-code-emacs-send-string`.
