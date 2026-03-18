;;; core-orgmode.el --- Org-mode configuration for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Updated: March 18, 2026
;; Keywords: lisp, org, productivity
;; Package-Requires: ((emacs "29.1") (org "9.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Comprehensive Org-mode configuration: global settings, TODO workflows,
;; agenda, skeletons, capture templates, export, Babel, and calendar.
;;
;; CHANGES (2026-03-18):
;;   - iCloud sync is duplex (org wins on conflict).
;;   - Removed org-capture-after-finalize-hook — too fragile, caused hangs.
;;     Push is now manual: C-c i from any contact entry pushes it to iCloud.
;;   - Birthday capture produces a full contact entry with BIRTHDAY field.
;;   - vdirsyncer config: conflict_resolution = "a wins" (local/org wins).
;;
;; CHANGES (2026-03-15):
;;   - Fixed Q and W capture templates.
;;   - Removed hugo and slimhtml from org-export-backends.

;;; Code:

;;;; ============================================================
;;;; Global settings
;;;; ============================================================

(setq org-startup-folded t)
(setq org-startup-align-all-tables t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;;;; ============================================================
;;;; Keybindings
;;;; ============================================================

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

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
;;;;
;;;; Birthday (b) produces a full contact entry with BIRTHDAY field,
;;;; same structure as Contact (c), so it syncs to iCloud as a proper
;;;; vCard with BDAY set rather than a standalone org date heading.
;;;; ============================================================

(defvar core-orgmode-contacts-template
  "** %^{Full name}
:PROPERTIES:
:EMAIL: %(org-contacts-template-email)
:PHONE: %^{Phone}
:ADDRESS: %^{Address}
:BIRTHDAY: %^{Birthday (yyyy-mm-dd)}
:NOTE: %^{Note}
:END:"
  "Template for capturing contact information.")

(defvar core-orgmode-birthday-template
  "** %^{Full name}
:PROPERTIES:
:EMAIL:
:PHONE:
:ADDRESS:
:BIRTHDAY: %^{Birthday (yyyy-mm-dd)}
:NOTE:
:END:"
  "Template for a birthday-focused contact entry.")

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
         (file+headline "~/Documents/org/capture/contacts.org" "Contacts")
         ,core-orgmode-birthday-template :empty-lines 1)
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
;;;; iCloud contacts sync via vdirsyncer — DUPLEX (org wins)
;;;;
;;;; vdirsyncer handles iCloud auth and .vcf file sync.
;;;; Org entries are matched to .vcf files via :VCARD_UID: property.
;;;;
;;;; SETUP (one-time):
;;;;   brew install vdirsyncer
;;;;   vdirsyncer discover && vdirsyncer sync
;;;;
;;;; ~/.config/vdirsyncer/config — note conflict_resolution = "a wins":
;;;;
;;;;   [general]
;;;;   status_path = "~/.vdirsyncer/status/"
;;;;
;;;;   [pair icloud_contacts]
;;;;   a = "icloud_contacts_local"
;;;;   b = "icloud_contacts_remote"
;;;;   collections = ["from b"]
;;;;   conflict_resolution = "a wins"
;;;;
;;;;   [storage icloud_contacts_local]
;;;;   type = "filesystem"
;;;;   path = "~/.vdirsyncer/contacts/"
;;;;   fileext = ".vcf"
;;;;
;;;;   [storage icloud_contacts_remote]
;;;;   type = "carddav"
;;;;   url = "https://contacts.icloud.com"
;;;;   username = "you@icloud.com"
;;;;   password.fetch = ["shell", "gpg --decrypt --quiet ~/.shh/.authinfo.gpg | awk '/contacts.icloud.com/{print $NF}'"]
;;;;
;;;; COMMANDS:
;;;;   M-x org-sync-icloud-contacts         pull: vcf → org
;;;;   M-x org-sync-icloud-contacts-full    pull: vdirsyncer sync, then vcf → org
;;;;   C-c i  (on a contact entry)          push: current entry → vcf → iCloud
;;;;   M-x org-push-icloud-contacts         push: all unpushed entries → iCloud
;;;; ============================================================

(defconst org-icloud-vcf-dir
  (expand-file-name "~/.vdirsyncer/contacts/card/")
  "Directory where vdirsyncer stores synced iCloud vCard files.")

(defconst org-icloud-contacts-file
  "~/Documents/org/capture/contacts.org"
  "Org file that receives the synced Contacts heading.")

(defconst org-icloud-contacts-heading "Contacts"
  "Top-level heading in contacts.org managed by the sync.")

;;; ── vCard parsing ────────────────────────────────────────────

(defun org-icloud--vcard-field (vcard field)
  "Return the first value of FIELD from VCARD string, or \"\".
Handles TYPE parameters e.g. EMAIL;TYPE=work:..."
  (catch 'found
    (dolist (line (split-string vcard "\n"))
      (let ((line (string-trim line)))
        (when (string-prefix-p field line)
          (let ((rest (substring line (length field))))
            (when (and (> (length rest) 0)
                       (or (string-prefix-p ":" rest)
                           (string-prefix-p ";" rest)))
              (let ((colon (string-search ":" line)))
                (when colon
                  (throw 'found (string-trim (substring line (1+ colon)))))))))))
    ""))

(defun org-icloud--vcard-multi (vcard field)
  "Return all values of FIELD in VCARD joined with \"; \"."
  (let (vals)
    (dolist (line (split-string vcard "\n"))
      (let ((line (string-trim line)))
        (when (string-prefix-p field line)
          (let ((rest (substring line (length field))))
            (when (and (> (length rest) 0)
                       (or (string-prefix-p ":" rest)
                           (string-prefix-p ";" rest)))
              (let ((colon (string-search ":" line)))
                (when colon
                  (push (string-trim (substring line (1+ colon))) vals))))))))
    (string-join (nreverse vals) "; ")))

(defun org-icloud--vcard-to-org (vcard)
  "Convert a raw VCARD string to an org heading entry string.
Stores UID as :VCARD_UID: for round-trip tracking."
  (let* ((name    (org-icloud--vcard-field vcard "FN"))
         (uid     (org-icloud--vcard-field vcard "UID"))
         (email   (org-icloud--vcard-multi  vcard "EMAIL"))
         (phone   (org-icloud--vcard-multi  vcard "TEL"))
         (bday    (org-icloud--vcard-field  vcard "BDAY"))
         (note    (replace-regexp-in-string
                   "[ \t]*\n[ \t]*" " "
                   (org-icloud--vcard-field vcard "NOTE")))
         (adr-raw (org-icloud--vcard-field  vcard "ADR"))
         (address (when (string-match ";;\\(.*\\)" adr-raw)
                    (string-join
                     (seq-filter (lambda (s) (not (string-empty-p s)))
                                 (split-string (match-string 1 adr-raw) ";"))
                     ", "))))
    (format "** %s\n:PROPERTIES:\n:EMAIL: %s\n:PHONE: %s\n:ADDRESS: %s\n:BIRTHDAY: %s\n:NOTE: %s\n:VCARD_UID: %s\n:ICLOUD_SYNC: %s\n:END:\n"
            (if (string-empty-p name) "Unknown" name)
            email phone (or address "") bday note
            (or uid "")
            (format-time-string "%Y-%m-%d"))))

;;; ── vCard generation ─────────────────────────────────────────

(defun org-icloud--make-vcard (name email phone address birthday note uid)
  "Generate a vCard 3.0 string.  Reuses UID if provided, generates one if not."
  (let ((uid (if (and uid (not (string-empty-p uid)))
                 uid
               (format "%s-%s"
                       (format-time-string "%Y%m%dT%H%M%S")
                       (substring (md5 (concat name (format-time-string "%s"))) 0 8)))))
    (mapconcat #'identity
               (delq nil
                     (list "BEGIN:VCARD"
                           "VERSION:3.0"
                           (concat "FN:" name)
                           (concat "N:"
                                   (let ((parts (reverse (split-string name " " t))))
                                     (mapconcat #'identity parts ";"))
                                   ";;;")
                           (unless (string-empty-p email) (concat "EMAIL:" email))
                           (unless (string-empty-p phone) (concat "TEL:" phone))
                           (unless (string-empty-p address) (concat "ADR:;;" address))
                           (unless (string-empty-p birthday)
                             (concat "BDAY:" (replace-regexp-in-string "-" "" birthday)))
                           (unless (string-empty-p note) (concat "NOTE:" note))
                           (concat "UID:" uid)
                           "END:VCARD"))
               "\r\n")))

;;; ── push: current entry → vcf ────────────────────────────────

(defun org-icloud--push-entry-at-point ()
  "Write the contact entry at point as a .vcf file and run vdirsyncer async.
Sets :VCARD_UID: and :ICLOUD_SYNC: on the entry after writing."
  (unless (file-directory-p org-icloud-vcf-dir)
    (user-error "vCard dir not found: %s — run vdirsyncer discover first"
                org-icloud-vcf-dir))
  (let* ((name     (org-get-heading t t t t))
         (email    (or (org-entry-get nil "EMAIL")    ""))
         (phone    (or (org-entry-get nil "PHONE")     ""))
         (address  (or (org-entry-get nil "ADDRESS")   ""))
         (birthday (or (org-entry-get nil "BIRTHDAY")  ""))
         (note     (or (org-entry-get nil "NOTE")      ""))
         (uid      (or (org-entry-get nil "VCARD_UID") ""))
         (vcard    (org-icloud--make-vcard name email phone address birthday note uid))
         ;; Extract the UID that ended up in the vCard
         (actual-uid (when (string-match "UID:\\([^\r\n]+\\)" vcard)
                       (string-trim (match-string 1 vcard))))
         (vcf-path  (concat org-icloud-vcf-dir actual-uid ".vcf")))
    (write-region (concat vcard "\r\n") nil vcf-path nil 'silent)
    (org-entry-put nil "VCARD_UID"   actual-uid)
    (org-entry-put nil "ICLOUD_SYNC" (format-time-string "%Y-%m-%d"))
    (when (executable-find "vdirsyncer")
      (start-process "vdirsyncer-push" "*vdirsyncer*" "vdirsyncer" "sync"))
    (message "Pushed \"%s\" to iCloud." name)))

(defun org-icloud-push-contact-at-point ()
  "Push the contact entry at point to iCloud.
Bound to C-c i in org-mode buffers visiting contacts.org."
  (interactive)
  (unless (string-equal (buffer-file-name)
                        (expand-file-name org-icloud-contacts-file))
    (user-error "Not in contacts.org"))
  (unless (= (org-current-level) 2)
    (user-error "Point is not on a contact entry (need level 2 heading)"))
  (org-icloud--push-entry-at-point))

(defun org-push-icloud-contacts ()
  "Push all contact entries under the Contacts heading to iCloud.
Skips entries that already have an up-to-date :ICLOUD_SYNC: date."
  (interactive)
  (unless (executable-find "vdirsyncer")
    (user-error "vdirsyncer not found — run: brew install vdirsyncer"))
  (let ((count 0))
    (with-current-buffer (find-file-noselect
                          (expand-file-name org-icloud-contacts-file))
      (org-map-entries
       (lambda ()
         (when (= (org-current-level) 2)
           (org-icloud--push-entry-at-point)
           (setq count (1+ count))))
       (concat "+LEVEL=2")
       nil))
    (message "Pushed %d contacts to iCloud." count)))

;;; ── keybinding for per-entry push ────────────────────────────

(defun org-icloud--maybe-bind-push ()
  "Bind C-c i to push-contact-at-point when visiting contacts.org."
  (when (and (buffer-file-name)
             (string-equal (buffer-file-name)
                           (expand-file-name org-icloud-contacts-file)))
    (local-set-key (kbd "C-c i") #'org-icloud-push-contact-at-point)))

(add-hook 'org-mode-hook #'org-icloud--maybe-bind-push)

;;; ── pull: vcf → org ──────────────────────────────────────────

(defun org-icloud--write-org (entries)
  "Replace the managed Contacts heading in contacts.org with ENTRIES."
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
             (heading  (concat "* " org-icloud-contacts-heading "\n"))
             (start    (string-search heading original))
             (updated
              (if start
                  (let ((next (string-search "\n* " original (1+ start))))
                    (concat (substring original 0 start)
                            block
                            (if next (substring original (1+ next)) "")))
                (concat (string-trim-right original) "\n\n" block "\n"))))
        (write-region updated nil file nil 'silent)))))

(defun org-sync-icloud-contacts ()
  "Convert vCards from `org-icloud-vcf-dir' into contacts.org.
Preserves :VCARD_UID: on each entry for duplex sync.
Run `org-sync-icloud-contacts-full' to pull from iCloud first."
  (interactive)
  (unless (file-directory-p org-icloud-vcf-dir)
    (user-error "vCard dir not found: %s\nRun: vdirsyncer discover && vdirsyncer sync"
                org-icloud-vcf-dir))
  (let* ((files   (directory-files org-icloud-vcf-dir t "\\.vcf\\'"))
         (vcards  (mapcar (lambda (f)
                            (with-temp-buffer
                              (insert-file-contents f)
                              (buffer-string)))
                          files))
         (entries (mapcar #'org-icloud--vcard-to-org vcards)))
    (org-icloud--write-org entries)
    (org-revert-all-org-buffers)
    (message "iCloud contacts synced -- %d contacts." (length entries))))

(defun org-sync-icloud-contacts-full ()
  "Run vdirsyncer to pull from iCloud, then convert vCards to org."
  (interactive)
  (unless (executable-find "vdirsyncer")
    (user-error "vdirsyncer not found — run: brew install vdirsyncer"))
  (message "Running vdirsyncer...")
  (let ((exit (call-process "vdirsyncer" nil "*vdirsyncer*" nil "sync")))
    (if (zerop exit)
        (org-sync-icloud-contacts)
      (message "vdirsyncer failed — see *vdirsyncer* buffer"))))

;;;; ============================================================
;;;; Finalization
;;;; ============================================================

(with-eval-after-load 'org-agenda
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo-all))))

(provide 'core-orgmode)

;;; core-orgmode.el ends here
