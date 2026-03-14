;;; core-settings.el --- General Emacs settings -*- lexical-binding: t; -*-

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
;; General Emacs settings: UI, encoding, file management, personalization,
;; and global utility keybindings that have no more specific home.
;;
;; CHANGES (2026-03-13):
;;   - Absorbed global utility bindings from core-keybindings.el (now deleted):
;;     C-c I (edit config), C-c r (revert buffer), M-o (other window).
;;   - Package-Requires minimum bumped from 27.1 to 29.1.

;;; Code:

;;;; ============================================================
;;;; Basic UI
;;;; ============================================================

(setq inhibit-startup-message t)
(menu-bar-mode 1)
(tool-bar-mode -1)
(setq scroll-step 1
      scroll-conservatively 10000)
(setq visible-bell t)
(global-visual-line-mode 1)
(column-number-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(delete-selection-mode 1)
(setq backward-delete-char-untabify-method 'hungry)

;;;; ============================================================
;;;; Encoding
;;;; ============================================================

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-multibyte t)

;;;; ============================================================
;;;; File management
;;;; ============================================================

(setq custom-file (make-temp-file "emacs-custom"))
(setq default-directory "~/projects/")
(add-to-list 'load-path (expand-file-name "~/projects/elisp"))
(setq create-lockfiles nil)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/per-save")))

(defun force-backup-of-buffer ()
  "Force a backup before saving: per-session on first save, per-save thereafter."
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook #'force-backup-of-buffer)
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;;;; ============================================================
;;;; Personalization
;;;; ============================================================

(setq user-full-name    "Cooper Oscarfono"
      user-mail-address "cooper@oscarfono.com")

(setq epg-gpg-program "/usr/bin/gpg2")
(require 'epa-file)
(epa-file-enable)
(setq auth-sources '("~/.shh/.authinfo.gpg"))
(setq auth-source-debug t)

;;;; ============================================================
;;;; Garbage collection
;;;; ============================================================

(setq gc-cons-threshold 10000000)
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold 1000000)))

;;;; ============================================================
;;;; Miscellaneous
;;;; ============================================================

(fset 'yes-or-no-p 'y-or-n-p)
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program  "firefox")
(subword-mode 1)

;;;; ============================================================
;;;; Global utility keybindings
;;;; ============================================================

(defun core-settings-edit-config ()
  "Open `~/.emacs.d/init.el' for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") #'core-settings-edit-config)
(global-set-key (kbd "C-c r") #'revert-buffer)
(global-set-key (kbd "M-o")   #'other-window)

(provide 'core-settings)

;;; core-settings.el ends here
