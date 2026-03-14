;;; core-erc.el --- ERC (IRC client) configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 13, 2026
;; Keywords: comm, irc, erc
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Configuration for ERC, Emacs' built-in IRC client.
;; Credentials are read from ~/.authinfo.gpg — no plaintext passwords here.
;;
;; Usage: M-x erc  (or C-c e from anywhere)
;;
;; To add a server entry to ~/.authinfo.gpg:
;;   machine irc.libera.chat login <nick> password <pass> port 6697

;;; Code:

(require 'erc)
(require 'erc-services)
(require 'erc-track)
(require 'erc-fill)
(require 'erc-match)

;;;; ============================================================
;;;; Identity
;;;; ============================================================

(setq erc-nick           "oscarfono"
      erc-user-full-name "Cooper Oscarfono"
      erc-email-userid   "cooper@oscarfono.com")

;;;; ============================================================
;;;; Connection defaults
;;;; ============================================================

(setq erc-server   "irc.libera.chat"
      erc-port     6697)

;; Read password from ~/.authinfo.gpg rather than prompting.
(setq erc-prompt-for-password nil)
(setq erc-prompt-for-nickserv-password nil)

(erc-services-mode 1)

(setq erc-nickserv-identify-mode 'autodetect)

;;;; ============================================================
;;;; Behaviour
;;;; ============================================================

(setq erc-kill-buffer-on-part t)       ; Close buffer when /part-ing a channel.
(setq erc-kill-server-buffer-on-quit t); Close server buffer on /quit.
(setq erc-auto-reconnect nil)          ; Don't silently reconnect.
(setq erc-join-buffer 'window)         ; Open channels in a new window.

;;;; ============================================================
;;;; Display
;;;; ============================================================

(erc-fill-mode 1)
(setq erc-fill-column 100)
(setq erc-fill-function 'erc-fill-static)
(setq erc-fill-static-center 20)       ; Width of the nick column.

(setq erc-timestamp-format "[%H:%M] "
      erc-insert-timestamp-function 'erc-insert-timestamp-left)

(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))  ; Reduce join/leave noise.

;;;; ============================================================
;;;; Tracking (mode line notifications)
;;;; ============================================================

(erc-track-mode 1)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
      erc-track-shorten-start 5)

;;;; ============================================================
;;;; Highlighting
;;;; ============================================================

(erc-match-mode 1)
(setq erc-keywords '())               ; Add keywords to highlight here.
(setq erc-current-nick-highlight-type 'nick)

;;;; ============================================================
;;;; Entry point
;;;; ============================================================

(defun core-erc-connect ()
  "Connect to IRC via ERC using credentials from ~/.authinfo.gpg."
  (interactive)
  (erc-tls :server erc-server
           :port   erc-port
           :nick   erc-nick))

;; Bind globally so it's reachable from any buffer.
(global-set-key (kbd "C-c e") #'core-erc-connect)

(provide 'core-erc)

;;; core-erc.el ends here
