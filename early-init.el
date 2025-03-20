;;; early-init.el --- Early initialization for Emacs with straight.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Keywords: lisp, tools, configuration
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; This file bootstraps `straight.el` and ensures it’s fully initialized...

;;; Code:

;; Load auth-sources for .authinfo.gpg
(require 'auth-source)
(setq auth-sources '("~/.authinfo.gpg"))
(setq straight-vc-git-default-protocol 'https)
(setq auth-source-debug t)  ; Log auth attempts

;; Bootstrap straight.el
(defvar bootstrap-version 6)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
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
(setq straight-vc-git-default-protocol 'https)  ; Prefer HTTPS
(setenv "GIT_ASKPASS" "")  ; No prompts
(setq straight-process-buffer "*straight-process*")  ; Log Git output

;; Install Org-mode early
(use-package org
  :straight (:type git :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git" :depth 1)
  :ensure t)

(provide 'early-init)
;;; early-init.el ends here
