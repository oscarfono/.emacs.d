;;; init.el --- Minimal Emacs configuration loader  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Keywords: lisp, configuration
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file serves as the primary entry point for my Emacs configuration.
;; It loads a custom Lisp directory and requires a set of core modules that
;; define the bulk of the configuration.  The goal is to keep this file minimal,
;; delegating detailed settings to separate files in `~/.emacs.d/lisp/'.

;;; Code:

;; (custom-set-variables '(custom-enabled-themes nil))  ; Disable all themes

;; Add custom Lisp directory to `load-path'.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; Load core configuration modules.
(require 'core-theme)
(require 'core-settings)
(require 'core-packages)
;; (require 'core-orgmode)
(require 'core-keybindings)

(provide 'init)

;;; init.el ends here
