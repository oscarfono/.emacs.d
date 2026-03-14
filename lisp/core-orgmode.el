;;; core-orgmode.el --- Org-mode configuration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Updated: March 15, 2026
;; Keywords: lisp, org, productivity
;; Package-Requires: ((emacs "29.1") (org "9.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Comprehensive Org-mode configuration: global settings, TODO workflows,
;; agenda, skeletons, capture templates, export, Babel, and calendar.
;;
;; CHANGES (2026-03-15):
;;   - Fixed Q and W capture templates (incorrect use of `plain' type/location).
;;   - Removed hugo and slimhtml from org-export-backends (not auto-registered).
;;   - Added org-sync-icloud-contacts command.

;;; Code:

;;;; ============================================================
;;;; Global settings
;;;; ============================================================

(setq org-startup-folded t)
(setq org-startup-align-all-tables t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;;; ============================================================
;;;; TODOs and priorities
;;;; ============================================================

(setq org-agenda-files (list "~/Documents/org/capture/todo.org"
                              "~/Documents/org/capture/contacts.org"))

(setq org-highest-priority ?A
      org-lowest-priority  ?E
      org-default-priority ?A)

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n/)" " >|< IN-PROGRESS(i!)" "⚠ WAIT(w@/!)"
                  "|" "DONE(d!)" "✘ KILL(k!)" "➰ PASS(p@/!)")))

(setq org-log-done 'time)

;;;; ============================================================
;;;; Agenda customization
;;;; ============================================================

(setq org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-deadline-warning-days 14
      org-agenda-span 'fortnight
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
      org-agenda-todo-ignore-deadlines 'all
      org-agenda-todo-ignore-scheduled 'all
      org-agenda-sorting-strategy
      '((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))

(add-hook 'after-init-hook (lambda () (org-agenda nil "a")))

;;;; ============================================================
;;;; Skeleton definitions
;;;; ============================================================

(define-skeleton core-orgmode-web-skeleton
  "Insert header info for web pages generated from an Org file."
  "\n "
  "#+TITLE: " str "\n\n"
  "#+DESCRIPTION: " str "\n"
  "#+SETUPFILE: " str "\n"
  "#+OPTIONS: num:nil ^:{} \n"
  "#+INCLUDE: './components/header.org' \n"
  "#+INCLUDE: './components/footer.org' \n")

(define-skeleton core-orgmode-blog-skeleton
  "Insert ox-hugo compatible header for a Sovereign Miner blog post."
  "Title: "
  "#+TITLE: " str "\n"
  "#+DATE: " (format-time-string "%Y-%m-%d") "\n"
  "#+AUTHOR: Cooper Oscarfono\n"
  "#+HUGO_BASE_DIR: ~/Projects/sovereign-miner/hugo\n"
  "#+HUGO_SECTION: posts\n"
  "#+HUGO_AUTO_SET_LASTMOD: t\n"
  "#+HUGO_DRAFT: true\n"
  "#+DESCRIPTION: " _ "\n"
  "#+HUGO_TAGS: \n"
  "#+HUGO_CATEGORIES: \n"
  "#+OPTIONS: num:nil ^:{}\n")

(define-skeleton core-orgmode-org-skeleton
  "Insert header info for a generic Org file."
  "\n "
  "#+TITLE: " str "\n"
  "#+AUTHOR: Cooper Oscarfono \n"
  "#+EMAIL: cooper@oscarfono.com \n")

(define-skeleton core-orgmode-apa-skeleton
  "Insert header info for APA-referenced documents from Org files."
  "\n "
  "#+TITLE: " str " \n"
  "#+OPTIONS: title:nil toc:nil H:4 author:nil date:nil TeX:t LaTeX:t \\n:t ^:nil \n"
  "#+EXPORT_SELECT_TAGS: export \n"
  "#+EXPORT_EXCLUDE_TAGS: noexport \n"
  "#+INCLUDE: './preamble.org' \n"
  "#+INCLUDE: './images/' \n")

(define-skeleton core-orgmode-ltr-skeleton
  "Insert LaTeX PDF letter template from an Org file."
  "\n"
  "#+LATEX_CLASS_OPTIONS: [a4paper,10pt] \n"
  "#+LATEX_HEADER: \\setlength{\\parskip}{1em} \\setlength{\\parindent}{0pt} \n"
  "#+LATEX_HEADER_EXTRA: \\pagenumbering{gobble} \n"
  "#+OPTIONS: toc:nil \n"
  "\n"
  "PO Box 19, \n"
  "Sandgate\n"
  "Queensland 4017 \n"
  "\n"
  "Dear sir/madam, \n"
  "\n"
  "_Re:_ \n"
  "\n"
  "Warm regards, \n"
  "\n"
  "#+ATTR_LATEX: :center nil :width 5cm \n"
  "[[file:~/DocumentsID/sodsig-001.png]] \n"
  "\n"
  "Cooper Oscarfono \n")

;;;; Skeleton keybindings
;;
;; Prefix: C-c S (capital S) — avoids shadowing `org-schedule' (C-c s).

(define-prefix-command 'core-orgmode-skeleton-map)
(global-set-key (kbd "C-c S") 'core-orgmode-skeleton-map)
(define-key core-orgmode-skeleton-map (kbd "w") #'core-orgmode-web-skeleton)
(define-key core-orgmode-skeleton-map (kbd "b") #'core-orgmode-blog-skeleton)
(define-key core-orgmode-skeleton-map (kbd "o") #'core-orgmode-org-skeleton)
(define-key core-orgmode-skeleton-map (kbd "a") #'core-orgmode-apa-skeleton)
(define-key core-orgmode-skeleton-map (kbd "l") #'core-orgmode-ltr-skeleton)

;;;; ============================================================
;;;; Capture templates
;;;; ============================================================

(defvar core-orgmode-contacts-template
  "** %^{contact}
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE: %^{XXX-XXX-XXXX}
:ADDRESS: %^{street name. city, state, postcode }
:BIRTHDAY: %^{dd-mm-yyyy}t
:NOTE: %^{NOTE}
:END:"
  "Template for capturing contact information.")

(defvar core-orgmode-project-template
  "* %^{Project Name}
:PROPERTIES:
:Customer Name: %^{Customer}
:Deadline: %^{dd-mm-yyyy}
:END"
  "Template for capturing project details.")

(defvar core-orgmode-expenses-template
  "* %^{expense}
:PROPERTIES:
:DATE: %U
:AMOUNT: %^{$0.00}
:PAID_TO: %^{company}
:PAYMENT_TYPE: %^{eftpos|cash|effort}
:END:"
  "Template for capturing expenses.")

(defvar core-orgmode-greatquotes-template
  "* %^{great quote here}
:PROPERTIES:
:QUOTE: %^{great quote}
:ATTRIBUTION: %?
:END"
  "Template for capturing notable quotes.")

(setq org-capture-templates
      `(("b" "Birthday" entry
         (file+headline "~/Documents/org/capture/contacts.org" "Birthdays")
         "* %^{Name}'s Birthday\n %^{Date of birth}T\n :PROPERTIES:\n :CATEGORY: birthday\n :END:\n"
         :empty-lines 1)
        ("c" "Contact" entry
         (file+headline "~/Documents/org/capture/contacts.org" "Contacts")
         ,core-orgmode-contacts-template :empty-lines 1)
        ("d" "Documentation" entry
         (file+headline "~/Documents/docs.org" "Documentation")
         "** %^{Subject}\n %^g\n %?\n %i\n Added %U")
        ("D" "Definition" entry
         (file+headline "~/Documents/org/capture/definitions.org" "Definitions")
         "** %^{Term} :: %^{Definition} ")
        ("e" "Expense" entry
         (file+olp+datetree "~/Documents/org/capture/expenses.org")
         ,core-orgmode-expenses-template :empty-lines 1)
        ("i" "Idea" entry
         (file+olp+datetree "~/Documents/org/capture/ideas.org" "Ideas")
         "** \ue910 %?\n I had this idea on %U\n %a" :empty-lines 1)
        ("j" "Journal" entry
         (file+olp+datetree "~/Documents/org/capture/journal.org")
         "* \ue916 %?\n Entered on %U\n" :empty-lines 1)
        ("l" "Lyric" entry
         (file+headline "~/Documents/org/capture/lyrical-ideas.org" "Lyrical Ideas Capture")
         "** %^{working-title}\n %^{verse}\n %^{hook}\n")
        ("p" "Project" entry
         (file+olp+datetree "~/Documents/org/capture/current-projects.org")
         ,core-orgmode-project-template :empty-lines 1)
        ("Q" "Quote" entry
         (file "~/Documents/org/capture/quotes.org")
         ,core-orgmode-greatquotes-template :empty-lines 1)
        ("r" "Read" entry
         (file+headline "~/Documents/org/capture/someday.org" "Read")
         "** %^{title}\n %^{author}" :empty-lines 1)
        ("s" "Subject" entry
         (file+headline "~/Documents/org/capture/someday.org" "Write")
         "** %^{subject}\n" :empty-lines 1)
        ("t" "Todo" entry
         (file+headline "~/Documents/org/capture/todo.org" "Tasks")
         "** TODO %?\n %i\n %a" :empty-lines 1)
        ("W" "Wishlist" entry
         (file+headline "~/Documents/org/capture/someday.org" "Wishlist")
         "** %^{thing}" :empty-lines 1)
        ("w" "Watch" entry
         (file+headline "~/Documents/org/capture/someday.org" "Watch")
         "** %^{movie title}\n %a" :empty-lines 1)))

;;;; ============================================================
;;;; Export configuration
;;;; ============================================================

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))

(add-to-list 'org-latex-classes
             '("article" "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(add-to-list 'org-latex-classes
             '("book" "\\documentclass{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))

(add-to-list 'org-latex-classes
             '("letter"
               "\\documentclass{letter} \\usepackage[margin=1in]{geometry} [NO-DEFAULT-PACKAGES] [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

(setq org-latex-listings 'minted
      org-latex-packages-alist '(("" "minted"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package ox-hugo
  :straight t
  :after ox
  :config
  (setq org-hugo-base-dir "~/Projects"))

(use-package ox-mediawiki
  :straight t
  :after ox)

(setq org-export-backends '(ascii html latex md mediawiki))

;;;; ============================================================
;;;; Babel configuration
;;;; ============================================================

(setq org-plantuml-jar-path
      (let ((local  (expand-file-name "~/.emacs.d/lib/plantuml.jar"))
            (global "/usr/local/share/plantuml/plantuml.jar"))
        (if (file-exists-p local) local global)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C          . t)
   (calc       . t)
   (clojure    . t)
   (css        . t)
   (ditaa      . t)
   (dot        . t)
   (emacs-lisp . t)
   (haskell    . t)
   (js         . t)
   (latex      . t)
   (lisp       . t)
   (ocaml      . t)
   (org        . t)
   (perl       . t)
   (plantuml   . t)
   (python     . t)
   (R          . t)
   (ruby       . t)
   (sass       . t)
   (scheme     . t)
   (shell      . t)
   (sql        . t)))

;;;; ============================================================
;;;; Mixed pitch (variable + fixed width in org buffers)
;;;; ============================================================

(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  (dolist (face '(org-table
                  org-block org-block-begin-line org-block-end-line
                  org-code org-verbatim org-formula
                  org-date org-special-keyword org-priority org-tag))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

;;;; ============================================================
;;;; Calendar — Kalgoorlie, Western Australia
;;;; ============================================================

(setq calendar-latitude  -30.748
      calendar-longitude 121.466)

(setq holiday-general-holidays  nil
      holiday-christian-holidays nil
      holiday-hebrew-holidays    nil
      holiday-islamic-holidays   nil
      holiday-bahai-holidays     nil
      holiday-oriental-holidays  nil)

;;;; ============================================================
;;;; iCloud contacts sync — pure Elisp, no external dependencies
;;;;
;;;; Credentials are read from ~/.shh/.authinfo.gpg:
;;;;   machine contacts.icloud.com login you@icloud.com password YOUR-APP-PASS
;;;;
;;;; Generate an app-specific password at appleid.apple.com.
;;;; iCloud requires one; your main Apple ID password won't work here.
;;;;
;;;; Usage: M-x org-sync-icloud-contacts
;;;; ============================================================

(require 'url)
(require 'dom)

(defconst org-icloud-carddav-base "https://contacts.icloud.com"
  "iCloud CardDAV base URL.")

(defconst org-icloud-contacts-file
  "~/Documents/org/capture/contacts.org"
  "Org file that receives the synced Contacts heading.")

(defconst org-icloud-contacts-heading "Contacts"
  "Heading managed by the sync — everything under it is replaced on each run.")

(defun org-icloud--credentials ()
  "Return (USER . PASSWORD) from ~/.shh/.authinfo.gpg for contacts.icloud.com."
  (let ((found (auth-source-search :host "contacts.icloud.com" :max 1)))
    (unless found
      (user-error
       "No entry for contacts.icloud.com in ~/.shh/.authinfo.gpg\n\
Add: machine contacts.icloud.com login you@icloud.com password YOUR-APP-PASS"))
    (let* ((entry (car found))
           (user  (plist-get entry :user))
           (pass  (let ((s (plist-get entry :secret)))
                    (if (functionp s) (funcall s) s))))
      (cons user pass))))

(defun org-icloud--propfind (url depth body user pass)
  "Send a PROPFIND request to URL with Depth DEPTH, XML BODY, and basic auth."
  (let* ((url-request-method "PROPFIND")
         (url-request-extra-headers
          `(("Depth"         . ,depth)
            ("Content-Type"  . "application/xml; charset=utf-8")
            ("Authorization" . ,(concat "Basic "
                                        (base64-encode-string
                                         (concat user ":" pass) t)))))
         (url-request-data (encode-coding-string body 'utf-8))
         (buf (url-retrieve-synchronously url t t 30)))
    (unless buf (error "PROPFIND to %s returned no response" url))
    (with-current-buffer buf
      (goto-char (point-min))
      (search-forward "\n\n" nil t)
      (decode-coding-string (buffer-substring (point) (point-max)) 'utf-8))))

(defun org-icloud--discover-addressbook (user pass)
  "Discover the iCloud addressbook URL for USER via CardDAV well-known lookup."
  ;; Step 1: /.well-known/carddav -> principal URL
  (let* ((resp1 (org-icloud--propfind
                 (concat org-icloud-carddav-base "/.well-known/carddav")
                 "0"
                 "<?xml version=\"1.0\"?>
<d:propfind xmlns:d=\"DAV:\">
  <d:prop><d:current-user-principal/></d:prop>
</d:propfind>"
                 user pass))
         (principal-path
          (when (string-match "<[^:>]*:?href[^>]*>\\([^<]+\\)<" resp1)
            (string-trim (match-string 1 resp1)))))
    (unless principal-path
      (error "CardDAV discovery: could not find principal URL"))

    ;; Step 2: principal -> addressbook-home-set
    (let* ((resp2 (org-icloud--propfind
                   (concat org-icloud-carddav-base principal-path)
                   "0"
                   "<?xml version=\"1.0\"?>
<d:propfind xmlns:d=\"DAV:\" xmlns:card=\"urn:ietf:params:xml:ns:carddav\">
  <d:prop><card:addressbook-home-set/></d:prop>
</d:propfind>"
                   user pass))
           (home-path
            (when (string-match
                   "addressbook-home-set[^>]*>[^<]*<[^:>]*:?href[^>]*>\\([^<]+\\)"
                   resp2)
              (string-trim (match-string 1 resp2)))))
      (unless home-path
        (error "CardDAV discovery: could not find addressbook-home-set"))
      (concat org-icloud-carddav-base home-path))))

(defun org-icloud--fetch-vcards (addressbook-url user pass)
  "Return a list of raw vCard strings from ADDRESSBOOK-URL."
  (let ((body (org-icloud--propfind
               addressbook-url "1"
               "<?xml version=\"1.0\"?>
<d:propfind xmlns:d=\"DAV:\" xmlns:card=\"urn:ietf:params:xml:ns:carddav\">
  <d:prop><card:address-data/></d:prop>
</d:propfind>"
               user pass))
        vcards)
    (with-temp-buffer
      (insert body)
      (goto-char (point-min))
      (while (re-search-forward "BEGIN:VCARD\\(\\(?:.\\|\n\\)*?\\)END:VCARD" nil t)
        (push (concat "BEGIN:VCARD" (match-string 1) "END:VCARD") vcards)))
    (nreverse vcards)))

(defun org-icloud--vcard-field (vcard field)
  "Return the first value of FIELD from raw VCARD string, or \"\"."
  (if (string-match (concat "^" field "[^:\r\n]*:\\(.*\\)$") vcard)
      (string-trim (match-string 1 vcard))
    ""))

(defun org-icloud--vcard-multi (vcard field)
  "Return all values of FIELD in VCARD joined with \"; \"."
  (let (vals)
    (with-temp-buffer
      (insert vcard)
      (goto-char (point-min))
      (while (re-search-forward (concat "^" field "[^:\r\n]*:\\(.*\\)$") nil t)
        (push (string-trim (match-string 1)) vals)))
    (string-join (nreverse vals) "; ")))

(defun org-icloud--vcard-to-org (vcard)
  "Convert a raw VCARD string to an org heading entry string."
  (let* ((name    (org-icloud--vcard-field vcard "FN"))
         (email   (org-icloud--vcard-multi  vcard "EMAIL"))
         (phone   (org-icloud--vcard-multi  vcard "TEL"))
         (bday    (org-icloud--vcard-field vcard "BDAY"))
         (note    (replace-regexp-in-string
                   "[\r\n]+" " "
                   (org-icloud--vcard-field vcard "NOTE")))
         ;; ADR: ;;street;city;region;postcode;country
         (adr-raw (org-icloud--vcard-field vcard "ADR"))
         (address (when (string-match ";;\\(.*\\)" adr-raw)
                    (string-join
                     (seq-filter (lambda (s) (not (string-empty-p s)))
                                 (split-string (match-string 1 adr-raw) ";"))
                     ", "))))
    (format "** %s\n:PROPERTIES:\n:EMAIL: %s\n:PHONE: %s\n:ADDRESS: %s\n:BIRTHDAY: %s\n:NOTE: %s\n:ICLOUD_SYNC: %s\n:END:\n"
            (if (string-empty-p name) "Unknown" name)
            email phone (or address "") bday note
            (format-time-string "%Y-%m-%d"))))

(defun org-icloud--write-org (entries)
  "Replace the managed heading in `org-icloud-contacts-file' with ENTRIES."
  (let* ((file   (expand-file-name org-icloud-contacts-file))
         (sorted (sort (copy-sequence entries)
                       (lambda (a b) (string< (downcase a) (downcase b)))))
         (block  (concat "* " org-icloud-contacts-heading "\n"
                         "# Synced from iCloud: "
                         (format-time-string "%Y-%m-%d %H:%M") "\n"
                         (string-join sorted "\n")
                         "\n")))
    (if (not (file-exists-p file))
        (progn
          (make-directory (file-name-directory file) t)
          (write-region block nil file))
      (let* ((original (with-temp-buffer
                         (insert-file-contents file)
                         (buffer-string)))
             (pattern  (concat "\\(\\* "
                               (regexp-quote org-icloud-contacts-heading)
                               "\\(?:\n\\|.\\)*?\\)"
                               "\\(?=^\\* \\|\\'\\)"))
             (updated  (if (string-match pattern original)
                           (replace-match block t t original)
                         (concat (string-trim-right original)
                                 "\n\n" block "\n"))))
        (write-region updated nil file nil 'silent)))))

(defun org-sync-icloud-contacts ()
  "Sync iCloud contacts into the Contacts heading of contacts.org.
Reads credentials from ~/.shh/.authinfo.gpg — see file header for format."
  (interactive)
  (message "Syncing iCloud contacts...")
  (condition-case err
      (let* ((creds   (org-icloud--credentials))
             (user    (car creds))
             (pass    (cdr creds))
             (ab-url  (org-icloud--discover-addressbook user pass))
             (vcards  (org-icloud--fetch-vcards ab-url user pass))
             (entries (mapcar #'org-icloud--vcard-to-org vcards)))
        (org-icloud--write-org entries)
        (org-revert-all-org-buffers)
        (message "iCloud contacts synced -- %d contacts." (length entries)))
    (error (message "iCloud sync failed: %s" (error-message-string err)))))

;;;; ============================================================
;;;; Keybindings
;;;; ============================================================

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

;;;; ============================================================
;;;; Finalization
;;;; ============================================================

(with-eval-after-load 'org-agenda
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo-all))))

(provide 'core-orgmode)

;;; core-orgmode.el ends here
