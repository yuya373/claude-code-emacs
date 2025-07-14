;;; install-deps.el --- Install dependencies for claude-code -*- lexical-binding: t; -*-

;;; Commentary:

;; This script installs the required dependencies for claude-code.
;; It can be run with: emacs -batch -l install-deps.el

;;; Code:

(require 'package)
(require 'cl-lib)

;; Add MELPA to package archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize package system
(package-initialize)

;; List of required packages
(defvar claude-code-required-packages
  '(projectile vterm transient markdown-mode lsp-mode websocket)
  "List of packages required by claude-code.")

;; Check if all packages are installed
(defun claude-code-all-packages-installed-p ()
  "Return t if all required packages are installed."
  (cl-every #'package-installed-p claude-code-required-packages))

;; Install missing packages
(defun claude-code-install-packages ()
  "Install required packages if they are not already installed."
  ;; Refresh package contents only if needed
  (unless (claude-code-all-packages-installed-p)
    (message "Refreshing package contents...")
    (package-refresh-contents))

  ;; Install each missing package
  (dolist (pkg claude-code-required-packages)
    (unless (package-installed-p pkg)
      (message "Installing %s..." pkg)
      (package-install pkg)))

  (message "All dependencies installed successfully!"))

;; Run installation
(claude-code-install-packages)

(provide 'install-deps)
;;; install-deps.el ends here
