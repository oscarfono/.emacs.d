;;; core-theme.el --- Configure themes and fonts for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 13, 2026
;; Keywords: faces, theme, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Installs and loads melancholy-theme from its GitHub repository via
;; straight.el.  Font configuration for the three font roles used by the
;; theme (monospace, sans-serif, handwriting) is also handled here.
;;
;; BREAKING CHANGES (2026-03-13):
;;   - Removed manual path construction, directory listing, and existence
;;     checks from :config — these were debug scaffolding using the private
;;     `straight--repos-dir' API.  straight.el handles all of this correctly
;;     on its own.
;;   - Package-Requires minimum bumped from 27.1 to 29.1.

;;; Code:

(use-package melancholy-theme
  :straight (:type git :host github :repo "oscarfono/melancholy-theme"
             :files ("*.el"))
  :ensure t
  :config
  (add-to-list 'custom-theme-load-path
               (straight--repos-dir "melancholy-theme"))
  (load-theme 'melancholy t))

(provide 'core-theme)

;;; core-theme.el ends here
