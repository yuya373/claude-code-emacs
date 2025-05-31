# Claude Code Emacs

An Emacs package to run Claude Code within Emacs.

## Starting Claude Code
`claude-code-emacs-run`

## Prompt Management Features
A `.claude-code-emacs.prompt.md` file is created in each project root.
The buffer is opened with `switch-to-buffer-other-window`.

### Key Bindings
- `C-c C-s`: Send the markdown section at point to Claude Code buffer
- `C-c C-r`: Send the selected region to Claude Code buffer
- `C-c C-o`: Open Claude Code session

## Transient Menus
### Main Menu
Display the main menu with `M-x claude-code-emacs-transient`

### Prompt Buffer Menu
In prompt buffers, display the menu with `C-c C-t` or `M-x claude-code-emacs-prompt-transient`

## Other Features
### Send Region
`claude-code-emacs-send-region` - Send the selected region or entire buffer to Claude Code

## Testing
### Running Tests
```bash
# Using Makefile
make test

# Or run directly
emacs -batch -l run-tests.el
```

### All Tasks (clean, compile, test)
```bash
make all
```

## Dependencies
- `projectile` - For project root detection
- `vterm` - For terminal emulation
- `transient` - For menu system
- `markdown-mode` - Base mode for prompt files

## Installation
Clone this repository and add to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/claude-code-emacs")
(require 'claude-code-emacs)

;; Optional: Set global keybinding for the main menu
(global-set-key (kbd "C-c c") 'claude-code-emacs-transient)
```

## Usage
1. Run `M-x claude-code-emacs-run` to start Claude Code in the current project
2. Use `M-x claude-code-emacs-open-prompt-file` to create/edit project-specific prompts
3. Access all commands through the transient menu with `C-c c`

## License
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.