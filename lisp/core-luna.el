;;; core-luna.el --- org-protocol links from the LUNA dashboard -*- lexical-binding: t; -*-

;; Author: Cooper Oscarfono
;; Keywords: org, notmuch, outlines

;;; Commentary:

;; The tab Today of the LUNA web page has links into Emacs (stage DB2).
;; The browser gives each org-protocol:// link to emacsclient.  These two
;; handlers open the place of the link:
;;
;;   org-protocol://luna-mail?query=Q
;;       Runs the notmuch query Q.  A thread or a message (thread:... or
;;       id:...) opens in `notmuch-show'.  Another query opens in
;;       `notmuch-search'.  notmuch runs on the VPS (core-mail.el).
;;
;;   org-protocol://luna-open?file=F&id=I&title=T&line=N
;;       Opens a heading of the LUNA memory.  The ID comes first.  Then
;;       the file F, relative to `core-luna-memory-root', and the heading
;;       with the title T.  The line N is the fallback.
;;
;; A web page can also open an org-protocol link.  Thus the handlers only
;; read: a notmuch query, or a file in the memory.  A file outside the
;; memory does not open.
;;
;; The desktop entry for the scheme org-protocol is in nyx (home/xdg.nix).

;;; Code:

(require 'org-protocol)

(declare-function notmuch-show "notmuch-show")
(declare-function notmuch-search "notmuch")

(defgroup core-luna nil
  "The links from the LUNA dashboard."
  :group 'org)

(defcustom core-luna-memory-root "~/org/luna/"
  "The clone of the LUNA memory on this laptop."
  :type 'directory)

(defun core-luna--value (info key)
  "The value of KEY in INFO, or nil when it is empty."
  (let ((value (plist-get info key)))
    (and (stringp value) (not (string-empty-p value)) value)))

(defun core-luna-mail (info)
  "Open the notmuch query of INFO, from a link of the LUNA dashboard."
  (let ((query (core-luna--value info :query)))
    (when query
      ;; core-mail gives the load path of notmuch and the command luna-notmuch.
      ;; A daemon that started before core-mail existed does not have them.
      (require 'core-mail)
      (require 'notmuch)
      (if (string-match-p "\\`\\(thread\\|id\\):[^ ]+\\'" query)
          (notmuch-show query)
        (notmuch-search query))))
  nil)

(defun core-luna--find-heading (title line)
  "Go to the heading with TITLE in this buffer.  Go to LINE when no heading has it."
  (goto-char (point-min))
  (cond
   ((and title (re-search-forward (concat "^\\*+ .*" (regexp-quote title)) nil t))
    (beginning-of-line))
   ((and line (> (string-to-number line) 0))
    (forward-line (1- (string-to-number line)))))
  (when (derived-mode-p 'org-mode)
    (org-fold-show-context)))

(defun core-luna-open (info)
  "Open the heading of INFO in the LUNA memory, from a link of the LUNA dashboard."
  (let* ((root (file-name-as-directory (expand-file-name core-luna-memory-root)))
         (id (core-luna--value info :id))
         (file (core-luna--value info :file))
         (path (and file (expand-file-name file root))))
    (cond
     ((and id (ignore-errors (org-id-goto id) t)))
     ((and path (file-in-directory-p path root) (file-exists-p path))
      (find-file path)
      (core-luna--find-heading (core-luna--value info :title) (core-luna--value info :line)))
     (t (message "LUNA: the link names no file in %s" root))))
  nil)

(dolist (entry '(("luna-mail" :protocol "luna-mail" :function core-luna-mail)
                 ("luna-open" :protocol "luna-open" :function core-luna-open)))
  (setq org-protocol-protocol-alist
        (cons entry (assoc-delete-all (car entry) org-protocol-protocol-alist))))

(provide 'core-luna)

;;; core-luna.el ends here
