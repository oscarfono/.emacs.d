;;; init.el --- init file configuration for the emacs text editor
;;

;;; Commentary:
;; This file loads Org-mode and then loads the rest of our Emacs initialization from Emacs Lisp
;; embedded in literate Org-mode files.

;;; Code:

;; Standard straight.el bootstrapping.
(defvar bootstrap-version
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; let's have straight used as default installer, but still retain the use of use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; give us verbose output for use-package when starting emacs with --debug-init flag
(if init-file-debug
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t
          debug-on-error t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

;; use latest version of org in place of bestowed version
;; this must come early in init to avoid issues.
(use-package org)

;; ensure required system packages are installed
(use-package use-package-ensure-system-package)

;; Set default org directory
(setq org-directory "~/src/org")

;; Set to the name of the file where new notes will be stored
;; (setq org-mobile-inbox-for-pull "~/src/org/mobile-org/flagged.org")

;; ;; Set to <your Dropbox root directory>/MobileOrg.
;; (setq org-mobile-directory "~/src/org/mobile-org")

;; now expand source blocks in my literate config.org file
(setq dotfiles-dir (file-name-directory (or (buffer-file-name) load-file-name)))

(let* ((org-dir (expand-file-name
                 "lisp" (expand-file-name
                         "org" (expand-file-name
                                "src" dotfiles-dir))))
       (org-contrib-dir (expand-file-name
                         "lisp" (expand-file-name
                                 "contrib" (expand-file-name
                                            ".." org-dir))))
       (load-path (append (list org-dir org-contrib-dir)
                          (or load-path nil))))
  ;; load up Org-mode and Org-babel
  (require 'ob-tangle))

;; load up all literate org-mode files in this directory
(mapc #'org-babel-load-file (directory-files dotfiles-dir t "\\.org$"))

(provide 'init)
;;; init.el ends here
;;; ============================================================
