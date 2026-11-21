;;; core-matrix.el --- Matrix client (ement.el) for LUNA -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cooper Oscarfono

;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: November 20, 2026
;; Keywords: comm, matrix, ement
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Configuration for ement.el, a Matrix client.  The homeserver is the
;; LUNA VPS, on the tailnet only.
;;
;; The host name is private, so this public file does not hold it.
;; Set it in ~/.emacs.d/local.el, which git ignores.  init.el loads
;; local.el before this module:
;;   (setq core-matrix-host "luna.<tailnet>.ts.net")
;;
;; The password comes from auth-sources: ~/.shh/.authinfo.gpg, set in
;; early-init.el.  This file has no password.
;; Add this line to ~/.shh/.authinfo.gpg:
;;   machine luna.<tailnet>.ts.net login @coops:luna.<tailnet>.ts.net password <pass>
;;
;; Usage: C-c m (or M-x core-matrix-connect).  The command connects, then
;; opens the room list.  Open the room "LUNA" to talk to LUNA.
;;
;; The "LUNA" room is not end-to-end encrypted.  ement.el cannot read
;; encrypted rooms, and LUNA cannot read them either.

;;; Code:

(require 'auth-source)

;;;; ============================================================
;;;; Identity
;;;; ============================================================

(defvar core-matrix-host nil
  "The LUNA homeserver, for example \"luna.<tailnet>.ts.net\".
Set it in local.el.  Tailscale must be up to reach it.")

(defun core-matrix-user-id ()
  "The Matrix user ID of the owner."
  (unless core-matrix-host
    (user-error "Set `core-matrix-host' in %s"
                (expand-file-name "local.el" user-emacs-directory)))
  (concat "@coops:" core-matrix-host))

;;;; ============================================================
;;;; Package
;;;; ============================================================

(use-package ement
  :defer t
  :config
  ;; Do not save the session token to disk.  Each connect reads the
  ;; password from auth-sources instead.
  ;; Do not set `ement-after-initial-sync-hook'.  Its default value opens
  ;; the room list and starts the idle timer.  The hook calls each
  ;; function with the session as its argument.
  (setq ement-save-sessions nil))

;;;; ============================================================
;;;; Entry point
;;;; ============================================================

(defun core-matrix-connect ()
  "Connect to the LUNA homeserver with the password from `auth-sources'."
  (interactive)
  (require 'ement)
  (let* ((user-id (core-matrix-user-id))
         (password (auth-source-pick-first-password
                    :host core-matrix-host
                    :user user-id)))
    (unless password
      (user-error "No password for %s in %s" user-id auth-sources))
    (ement-connect :user-id user-id
                   :password password
                   :uri-prefix (concat "https://" core-matrix-host))))

;; Bind globally so it's reachable from any buffer.
(global-set-key (kbd "C-c m") #'core-matrix-connect)

(provide 'core-matrix)
;;; core-matrix.el ends here
