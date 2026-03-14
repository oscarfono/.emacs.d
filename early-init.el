;;; early-init.el --- Early initialization for Emacs with straight.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 13, 2026
;; Keywords: lisp, tools, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Bootstraps straight.el and installs org-mode before init.el runs.
;; Auth-source is configured here because straight.el and org both need
;; credentials available before the rest of the config loads.
;;
;; CHANGES (2026-03-13):
;;   - Removed duplicate `straight-vc-git-default-protocol' declaration.
;;   - Removed duplicate `auth-source-debug' (now only in core-settings.el).
;;   - Package-Requires bumped to 29.1.

;;; Code:

;; Credentials live in ~/.shh/.authinfo.gpg — set once here, not again later.
(require 'auth-source)
(setq auth-sources '("~/.shh/.authinfo.gpg"))

;; Bootstrap straight.el
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

;; Configure straight.el
(setq straight-use-package-by-default t)
(require 'straight)
(straight-use-package 'use-package)
(setq straight-vc-git-default-protocol 'https)  ; Prefer HTTPS for all repos.
(setenv "GIT_ASKPASS" "")                        ; Suppress git credential prompts.
(setq straight-process-buffer "*straight-process*")

;; Install Org-mode early so core-orgmode.el gets the straight-managed version,
;; not the Emacs built-in. Must happen before any (require 'org).
(use-package org
  :straight (:host nil :type git
             :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
             :depth 1)
  :ensure t)

(provide 'early-init)

;;; early-init.el ends here
