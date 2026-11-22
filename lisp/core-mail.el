;;; core-mail.el --- notmuch over SSH: the mail on LUNA -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Cooper Oscarfono

;; Author: Cooper Oscarfono
;; Created: November 22, 2026
;; Keywords: mail, notmuch
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; notmuch.el reads the mail on the LUNA VPS.  The mail stays on the VPS.
;; Emacs runs each notmuch command there, through the script
;; `luna-notmuch' on this laptop (the home-manager module
;; mail-notmuch of the LUNA flake).  The script uses Tailscale SSH.
;;
;; LUNA sorts the mail in code and gives tags (stage I2 of LUNA):
;;   attention  the subject asks for an action (overdue, security alert)
;;   rsvp       a calendar invitation: say yes or no
;;   cull       junk and marketing: a tag only, nothing is deleted
;;   receipt invoice bill statement payslip travel legal notice unsorted
;;
;; A tag change here changes the notmuch database on the VPS.  It does
;; not change the mailbox at iCloud or Proton.
;;
;; CAUTION: the fetch on the VPS locks the database for some seconds
;; each 10 minutes.  A tag change at that time gives the error "Unable
;; to get write lock".  Do the tag change again.
;;
;; Usage: C-c M (or M-x notmuch).

;;; Code:

(use-package notmuch
  :straight t
  :commands (notmuch notmuch-search notmuch-tree)
  ;; C-c n is the prefix of org-roam, and C-c m is Matrix.
  :bind ("C-c M" . notmuch)
  :custom
  (notmuch-command "luna-notmuch")
  ;; The newest mail first.
  (notmuch-search-oldest-first nil)
  (notmuch-show-all-tags-list t)
  (notmuch-saved-searches
   '((:name "attention" :query "tag:attention and not tag:cull" :key "a")
     (:name "rsvp"      :query "tag:rsvp" :key "r")
     (:name "money"     :query "tag:receipt or tag:invoice or tag:bill or tag:statement or tag:payslip" :key "m")
     (:name "travel"    :query "tag:travel" :key "t")
     (:name "legal"     :query "tag:legal" :key "l")
     (:name "notices"   :query "tag:notice" :key "o")
     (:name "unsorted"  :query "tag:unsorted" :key "u")
     ;; New mail on the VPS has the tag new, not inbox.
     (:name "new"       :query "tag:new and not tag:cull" :key "n")
     (:name "cull"      :query "tag:cull" :key "c")
     (:name "all mail"  :query "*" :key "*"))))

(provide 'core-mail)

;;; core-mail.el ends here
