;;; core-theme.el --- Configure themes and fonts for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Keywords: faces, theme, configuration
;; Package-Requires: ((emacs "27.1") (straight "0") (use-package "0"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file configures Emacs themes and fonts, specifically installing and
;; loading the `melancholy-theme' from its GitHub repository using `straight.el'
;; and `use-package'.  It ensures the theme is available and applied during
;; Emacs initialization, with diagnostic messages to aid debugging.

;;; Code:

;; Install and configure the melancholy theme.
(use-package melancholy-theme
  :straight (:type git
             :host github
             :repo "oscarfono/melancholy-theme"
             :files ("*.el" "*.elc"))
  :ensure t
  :no-require t             ; Theme files are not libraries; do not `require'.
  :config
  (let ((theme-dir (straight--repos-dir "melancholy-theme"))
        (theme-file "melancholy-theme.el"))
    (message "Theme directory: %s" theme-dir)
    (message "Files in theme directory: %s" (directory-files theme-dir t))
    (let ((full-path (expand-file-name theme-file theme-dir)))
      (if (file-exists-p full-path)
          (progn
            (message "Theme file exists: %s" full-path)
            (message "File readable? %s" (file-readable-p full-path))
            (load full-path nil t)  ; Load the theme file silently.
            (message "Theme provided? %s" (featurep 'melancholy-theme))
            (add-to-list 'custom-theme-load-path theme-dir)
            (message "Custom theme load path: %s" custom-theme-load-path)
            (message "Available themes: %s" (custom-available-themes))
            (message "Loading melancholy theme...")
            (load-theme 'melancholy t t)  ; Load without prompting.
            (enable-theme 'melancholy)    ; Explicitly enable the theme
            (message "Theme loaded successfully"))
        (message "Theme file missing: %s" full-path)))))

(provide 'core-theme)

;;; core-theme.el ends here
