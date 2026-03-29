;;; core-settings.el --- General Emacs settings -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 29, 2026
;; Keywords: lisp, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; General Emacs settings: UI, encoding, file management, personalization,
;; and global utility keybindings that have no more specific home.
;;
;; Runs on both macOS and Linux.  Platform-specific branches are clearly marked.
;;
;; CHANGES (2026-03-29):
;; - Removed global default-directory: was causing Magit (C-x g) to open
;;   ~/projects/ instead of the current buffer's repo.
;; - Redirect auto-save files (#...#) to ~/.emacs.d/auto-save/.
;; - Redirect undo-tree history files to ~/.emacs.d/undo/.
;;
;; CHANGES (2026-03-13):
;; - Removed auth-sources (now set once in early-init.el).
;; - Removed auth-source-debug (set in early-init.el, disabled after init).
;; - Fixed subword-mode: was called at top level, now hooked to prog-mode.
;; - Added cross-platform browser and GPG configuration.
;; - Absorbed utility keybindings from deleted core-keybindings.el.
;; - Package-Requires bumped to 29.1.

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

;; subword-mode is buffer-local — hook it, don't call it at top level.
(add-hook 'prog-mode-hook #'subword-mode)

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

;; Discard customize writes — config lives in version control only.
(setq custom-file (make-temp-file "emacs-custom"))

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

;; Redirect auto-save files (#...#) away from project directories.
(let ((auto-save-dir (expand-file-name "~/.emacs.d/auto-save/")))
  (make-directory auto-save-dir :parents)
  (setq auto-save-file-name-transforms
        `((".*" ,auto-save-dir t))))

;; Redirect undo-tree history files to the dedicated undo directory.
(setq undo-tree-history-directory-alist
      `(("." . ,(expand-file-name "~/.emacs.d/undo/"))))

;;;; ============================================================
;;;; Personalization
;;;; ============================================================

(setq user-full-name "Cooper Oscarfono"
      user-mail-address "cooper@oscarfono.com")

;;;; ============================================================
;;;; GPG — platform-aware
;;;; ============================================================

(require 'epa-file)
(epa-file-enable)
(setq epg-gpg-program
      (cond
       ((eq system-type 'darwin)
        ;; Homebrew gpg on macOS: brew install gnupg
        (or (executable-find "gpg2")
            (executable-find "gpg")
            "/usr/local/bin/gpg"))
       (t
        ;; Linux standard path
        "/usr/bin/gpg2")))

;; Disable pinentry passthrough in GUI — lets gpg-agent handle the prompt.
(setq epa-pinentry-mode 'loopback)

;;;; ============================================================
;;;; Browser — platform-aware
;;;; ============================================================

(setq browse-url-browser-function
      (cond
       ((eq system-type 'darwin) 'browse-url-default-macosx-browser)
       (t 'browse-url-generic)))

(when (eq system-type 'gnu/linux)
  (setq browse-url-generic-program
        (or (executable-find "brave-browser")
            (executable-find "firefox")
            (executable-find "chromium"))))

;;;; ============================================================
;;;; Garbage collection
;;;; ============================================================

(setq gc-cons-threshold 10000000)
(add-hook 'after-init-hook
          (lambda ()
            ;; Disable auth-source debug logging after init completes.
            (setq auth-source-debug nil)
            (setq gc-cons-threshold 16777216) ; 16mb working value
            (setq gc-cons-percentage 0.1)))

;;;; ============================================================
;;;; Miscellaneous
;;;; ============================================================

(fset 'yes-or-no-p 'y-or-n-p)

;; Emacs server: start if not already running, so emacsclient always works.
(require 'server)
(unless (server-running-p)
  (server-start))

;;;; ============================================================
;;;; Global utility keybindings
;;;; ============================================================

(defun core-settings-edit-config ()
  "Open `~/.emacs.d/init.el' for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(global-set-key (kbd "C-c I") #'core-settings-edit-config)
(global-set-key (kbd "C-c r") #'revert-buffer)
(global-set-key (kbd "M-o") #'other-window)

(provide 'core-settings)
;;; core-settings.el ends here
