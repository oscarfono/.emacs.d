;;; core-keybindings.el --- Custom keybindings for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Keywords: lisp, keybindings, convenience
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file defines custom keybindings and associated functions for my Emacs
;; configuration.  It includes shortcuts for editing the configuration file,
;; starting ERC (Emacs IRC client), and various Org-mode commands.  These bindings
;; are intended to enhance workflow efficiency.

;;; Code:

(defun core-keybindings-edit-config ()
  "Edit the custom Emacs configuration file.
Opens `~/.emacs.d/init.el' in a buffer for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun core-keybindings-start-erc ()
  "Start ERC and load its configuration.
Loads `erc-config.el' after requiring the ERC package."
  (interactive)
  (require 'erc)
  (load "erc-config.el" t))  ; `t' means no error if file is missing.

;; Define global keybindings
(global-set-key (kbd "C-c I") #'core-keybindings-edit-config)  ; Edit config file.
(global-set-key (kbd "C-c e") #'core-keybindings-start-erc)    ; Start ERC.
(global-set-key (kbd "C-c r") #'revert-buffer)                 ; Revert buffer.
(global-set-key (kbd "M-o") #'other-window)                    ; Switch to other window.
(global-set-key (kbd "C-c l") #'org-store-link)                ; Store Org link.
(global-set-key (kbd "C-c a") #'org-agenda)                    ; Open Org agenda.
(global-set-key (kbd "C-c c") #'org-capture)                   ; Start Org capture.
(global-set-key (kbd "C-S-<f2>") #'core-orgmode-web-skeleton)    ; Insert web skeleton.
(global-set-key (kbd "C-S-<f3>") #'core-orgmode-blog-skeleton)   ; Insert blog skeleton.
(global-set-key (kbd "C-S-<f4>") #'core-orgmode-org-skeleton)    ; Insert Org skeleton.
(global-set-key (kbd "C-S-<f5>") #'core-orgmode-apa-skeleton)    ; Insert APA skeleton.
(global-set-key (kbd "C-S-<f6>") #'core-orgmode-ltr-skeleton)    ; Insert letter skeleton.


(provide 'core-keybindings)

;;; core-keybindings.el ends here
