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
;; Installs and loads melancholy-theme via straight.el.
;;
;; CHANGES (2026-03-13):
;;   - Removed manual path construction and straight--repos-dir (private API).
;;     straight.el adds the repo to load-path automatically; custom-theme-load-path
;;     only needs the directory straight already manages.

;;; Code:

(use-package melancholy-theme
  :straight (:type git :host github :repo "oscarfono/melancholy-theme"
             :files ("*.el"))
  :ensure t
  :config
  ;; straight puts the repo on load-path, but custom-theme-load-path is separate.
  (add-to-list 'custom-theme-load-path
               (file-name-directory
                (locate-file "melancholy-theme.el" load-path)))
  (load-theme 'melancholy t))

(provide 'core-theme)

;;; core-theme.el ends here
