;;; core-settings.el --- General Emacs settings  -*- lexical-binding: t; -*-

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
;; This file defines general settings for Emacs, including UI preferences,
;; encoding, file management, personalization, and miscellaneous tweaks.
;; It is part of a modular configuration loaded by `init.el'.

;;; Code:

;; Basic UI settings.
(setq inhibit-startup-message t)      ; Disable startup screen.
(menu-bar-mode 1)                     ; Enable menu bar, unlike previous setups.
(tool-bar-mode -1)                    ; Disable tool bar.
(scroll-bar-mode -1)                  ; Disable scroll bars.
(setq scroll-step 1                   ; Scroll one line at a time.
      scroll-conservatively 10000)    ; Smooth scrolling.
(setq visible-bell t)                 ; Use visible bell instead of audible.
(global-visual-line-mode 1)           ; Enable word wrap globally.
(column-number-mode 1)                ; Show column numbers in mode line.
(add-hook 'prog-mode-hook #'display-line-numbers-mode) ; Line numbers in programming modes.
(delete-selection-mode 1)             ; Allow deleting selected text.
(setq backward-delete-char-untabify-method 'hungry) ; Backspace deletes tabs aggressively.

;; Encoding settings.
(setq locale-coding-system 'utf-8)    ; Set locale to UTF-8.
(set-terminal-coding-system 'utf-8)   ; Terminal encoding.
(set-keyboard-coding-system 'utf-8)   ; Keyboard encoding.
(set-selection-coding-system 'utf-8)  ; Selection encoding.
(prefer-coding-system 'utf-8)         ; Prefer UTF-8 for all coding systems.
(set-language-environment 'utf-8)     ; Set language environment to UTF-8.
(set-default-coding-systems 'utf-8)   ; Default coding system.
(set-buffer-multibyte t)              ; Enable multibyte characters.

;; File management settings.
(setq custom-file (make-temp-file "emacs-custom")) ; Temporary custom file.
(setq default-directory "~/projects/") ; Default working directory.
(add-to-list 'load-path (expand-file-name "~/projects/elisp")) ; Add custom elisp dir.
(setq create-lockfiles nil)           ; Disable lock files.
(setq backup-directory-alist '(("." . "~/.emacs.d/backups/per-save"))) ; Backup directory.

(defun force-backup-of-buffer ()
  "Force a backup of the current buffer before saving.
This creates a per-session backup on the first save and per-save backups thereafter."
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook #'force-backup-of-buffer) ; Custom backup function.
(add-hook 'before-save-hook #'delete-trailing-whitespace) ; Clean trailing whitespace.
(setq-default indent-tabs-mode nil)   ; Use spaces instead of tabs.

;; Personalization settings.
(setq user-full-name "Cooper Oscarfono"
      user-mail-address "cooper@oscarfono.com") ; User identity.
(setq epg-gpg-program "/usr/bin/gpg2") ; GPG binary path.
(require 'epa-file)                   ; Load EasyPG for file encryption.
(epa-file-enable)                     ; Enable encrypted file handling.
(setq auth-sources '("~/.shh/.authinfo.gpg")) ; Authentication source.
(setq auth-source-debug t)            ; Debug authentication.

;; Garbage collection settings.
(setq gc-cons-threshold 10000000)     ; Increase GC threshold during startup.
(add-hook 'after-init-hook
          (lambda ()
            "Restore garbage collection threshold after initialization."
            (setq gc-cons-threshold 1000000)))

;; Miscellaneous settings.
(fset 'yes-or-no-p 'y-or-n-p)         ; Replace yes/no with y/n prompts.
(setq browse-url-browser-function 'browse-url-generic ; Use generic browser function.
      browse-url-generic-program "firefox") ; Set Firefox as default browser.
(subword-mode 1)                      ; Enable subword navigation.

(provide 'core-settings)

;;; core-settings.el ends here
