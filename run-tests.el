;;; run-tests.el --- Test runner for claude-code-emacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Run this file to execute all tests:
;; emacs -batch -l run-tests.el

;;; Code:

;; Add current directory to load path
(add-to-list 'load-path (file-name-directory load-file-name))
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-install 'ert)
(package-install 'projectile)
(package-install 'vterm)
(package-install 'transient)
(package-install 'markdown-mode)

;; Load dependencies
(require 'ert)
(require 'projectile)
(require 'vterm)
(require 'transient)
(require 'markdown-mode)

;; Load the package
(require 'claude-code-emacs)

;; Load tests
(load-file "test-claude-code-emacs.el")

;; Run all tests
(ert-run-tests-batch-and-exit)

;;; run-tests.el ends here
