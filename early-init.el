;;; early-init.el --- Early initialization for Emacs with straight.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 14, 2026
;; Keywords: lisp, tools, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Bootstraps straight.el and installs org-mode before init.el runs.
;;
;; Startup optimisations applied here (highest leverage — runs before GUI):
;;   1. gc-cons-threshold maximised for duration of startup, restored after.
;;   2. file-name-handler-alist cleared for duration of startup, restored after.
;;      This prevents every file load being checked against remote/archive regexps.
;;   3. package.el disabled — straight.el is the sole package manager.
;;
;; CHANGES (2026-03-14):
;;   - Added startup GC, file-handler, and package.el optimisations.

;;; Code:

;;;; ============================================================
;;;; Startup performance — must be first
;;;; ============================================================

;; Maximise GC threshold for the duration of startup.
;; Prevents GC pauses while packages load. Restored by after-init-hook
;; in core-settings.el.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Suppress file handler checks (remote files, archives, etc.) during startup.
;; Every file loaded during init would otherwise be checked against this list.
(defvar my/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist my/file-name-handler-alist)))

;; Disable package.el — straight.el handles everything.
(setq package-enable-at-startup nil)

;;;; ============================================================
;;;; Credentials
;;;; ============================================================

;; Set once here — straight.el and org both need auth before init.el loads.
(require 'auth-source)
(setq auth-sources '("~/.shh/.authinfo.gpg"))

;;;; ============================================================
;;;; straight.el bootstrap
;;;; ============================================================

(defvar bootstrap-version 6)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (message "Bootstrapping straight.el...")
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp))
    (message "Bootstrapping straight.el...done"))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(require 'straight)
(straight-use-package 'use-package)
(setq straight-vc-git-default-protocol 'https)
(setenv "GIT_ASKPASS" "")
(setq straight-process-buffer "*straight-process*")

;;;; ============================================================
;;;; Org-mode — installed early so core-orgmode.el gets the
;;;; straight-managed version, not the Emacs built-in.
;;;; Must happen before any (require 'org).
;;;; ============================================================

(use-package org
  :straight (:host nil :type git
             :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
             :depth 1)
  :ensure t)

(provide 'early-init)

;;; early-init.el ends here
