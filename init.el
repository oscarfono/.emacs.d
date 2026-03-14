;;; init.el --- Minimal Emacs configuration loader -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 13, 2026
;; Keywords: lisp, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Primary entry point.  Delegates all configuration to modules in
;; `~/.emacs.d/lisp/'.
;;
;; Load order matters:
;;   core-theme    — loaded first so the UI is correct before other packages init
;;   core-settings — base Emacs behaviour, encoding, file management
;;   core-packages — third-party packages via straight.el
;;   core-orgmode  — Org-mode configuration
;;   core-erc      — ERC (IRC client) configuration
;;
;; CHANGES (2026-03-13):
;;   - Removed core-keybindings (bindings distributed to owning modules).
;;   - Added core-erc.
;;   - Package-Requires minimum bumped from 27.1 to 29.1.

;;; Code:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-theme)
(require 'core-settings)
(require 'core-packages)
(require 'core-orgmode)
(require 'core-erc)

(provide 'init)

;;; init.el ends here
