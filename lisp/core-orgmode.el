;;; core-orgmode.el --- Org-mode configuration for Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Keywords: lisp, org, productivity
;; Package-Requires: ((emacs "27.1") (org "9.7"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file provides a comprehensive configuration for Org-mode, including
;; global settings, TODO workflows, agenda customization, skeleton definitions,
;; capture templates, export options, Babel language support, calendar settings,
;; and keybindings. It is designed to work with the latest Org-mode version
;; installed via `straight.el' from `early-init.el'.

;;; Code:

;;;; Global settings

(setq org-startup-folded t)              ; Collapse headlines on file open.
(setq org-startup-align-all-tables t)    ; Align tables when opening files.
(setq org-clock-persist 'history)        ; Persist clock history across sessions.
(org-clock-persistence-insinuate)        ; Enable clock persistence mechanism.

;;;; TODOs and priorities

(setq org-agenda-files (list "~/capture/todo.org")) ; Files for agenda tracking.
(setq org-highest-priority ?A)           ; Highest TODO priority.
(setq org-lowest-priority ?E)            ; Lowest TODO priority.
(setq org-default-priority ?A)           ; Default TODO priority.
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n/)" " >|< IN-PROGRESS(i!)" "⚠ WAIT(w@/!)"
                  "|" "DONE(d!)" "✘ KILL(k!)" "➰ PASS(p@/!)")))
;; Custom TODO sequence with logging.
(setq org-log-done 'time)                ; Log completion time for DONE tasks.

;;;; Agenda customization

(setq org-agenda-include-diary t)        ; Include Emacs diary in agenda.
(setq org-agenda-window-setup 'current-window) ; Open agenda in current window.
(setq org-deadline-warning-days 14)      ; Warn 14 days before deadlines.
(setq org-agenda-span 'fortnight)        ; Show two weeks in agenda.
(setq org-agenda-skip-scheduled-if-deadline-is-shown t) ; Skip duplicates.
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;; No prewarning if task is scheduled.
(setq org-agenda-todo-ignore-deadlines 'all) ; Ignore deadlines in TODO list.
(setq org-agenda-todo-ignore-scheduled 'all) ; Ignore scheduled in TODO list.
(setq org-agenda-sorting-strategy
      '((agenda deadline-up priority-down)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep)))
;; Sorting strategy for agenda views.
(add-hook 'after-init-hook
          (lambda () (org-agenda nil "a"))) ; Display agenda on startup.

;;;; Skeleton definitions

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
  "Insert header info for a blog file."
  "\n "
  "#+TITLE: " str "\n\n"
  "#+DATE: " (current-time-string) "\n\n"
  "#+AUTHOR: Cooper Oscarfono \n\n")

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
  "QLD 9348 \n"
  "\n"
  "Dear sir/madam, \n"
  "\n"
  "_Re:_ \n"
  "\n"
  "Warm regards, \n"
  "\n"
  "#+ATTR_LATEX: :center nil :width 5cm \n"
  "[[file:~/ID/sodsig-001.png]] \n"
  "\n"
  "Cooper Oscarfono \n")

;;;; Capture templates

(defvar core-orgmode-contacts-template
  "** %^{contact}
    :PROPERTIES:
      :EMAIL: %(org-contacts-template-email)
      :PHONE: %^{XXX-XXX-XXXX}
      :ADDRESS: %^{street name. city, postcode NZ}
      :BIRTHDAY: %^{dd-mm-yyyy}t
      :NOTE: %^{NOTE}
    :END:"
  "Template for capturing contact information in Org-mode.")

(defvar core-orgmode-project-template
  "* %^{Project Name}
   :PROPERTIES:
     :Customer Name: %^{Customer}
     :Deadline: %^{dd-mm-yyyy}
   :END"
  "Template for capturing project details in Org-mode.")

(defvar core-orgmode-expenses-template
  "* %^{expense}
    :PROPERTIES:
      :DATE: %U
      :AMOUNT: %^{$0.00}
      :PAID_TO: %^{company}
      :PAYMENT_TYPE: %^{eftpos|cash|effort}
    :END:"
  "Template for capturing expenses in Org-mode.")

(defvar core-orgmode-greatquotes-template
  "* %^{great quote here}
   :PROPERTIES:
     :QUOTE: %^{great quote}
     :ATTRIBUTION: %?
   :END"
  "Template for capturing notable quotes in Org-mode.")

(setq org-capture-templates
      `(("c" "Contact" entry
         (file+headline "~/capture/contacts.org" "Contacts")
         ,core-orgmode-contacts-template :empty-lines 1)
        ("d" "Documentation" entry
         (file+headline "~/Documents/docs.org" "Documentation")
         "** %^{Subject}\n %^g\n %?\n %i\n Added %U")
        ("D" "Definition" entry
         (file+headline "~/capture/definitions.org" "Definitions")
         "** %^{Term} :: %^{Definition} ")
        ("e" "Expense" entry
         (file+olp+datetree "~/capture/expenses.org")
         ,core-orgmode-expenses-template :empty-lines 1)
        ("i" "Idea" entry
         (file+olp+datetree "~/capture/ideas.org" "Ideas")
         "** \u{e910} %?\n I had this idea on %U\n %a" :empty-lines 1)
        ("j" "Journal" entry
         (file+olp+datetree "~/capture/journal.org")
         "* \u{e916} %?\n Entered on %U\n" :empty-lines 1)
        ("l" "Lyric" entry
         (file+headline "~/capture/lyrical-ideas.org" "Lyrical Ideas Capture")
         "** %^{working-title}\n %^{verse}\n %^{hook}\n")
        ("p" "Project" entry
         (file+olp+datetree "~/Public/projects/oscarfono/projects/current-projects.org")
         ,core-orgmode-project-template :empty-lines 1)
        ("Q" "Quote" entry
         (plain "~/capture/quotes.org")
         ,core-orgmode-greatquotes-template :empty-lines 1)
        ("r" "Read" entry
         (file+headline "~/capture/someday.org" "Read")
         "** %^{title}\n %^{author}" :empty-lines 1)
        ("s" "Subject" entry
         (file+headline "~/capture/someday.org" "Write")
         "** %^{subject}\n" :empty-lines 1)
        ("t" "Todo" entry
         (file+headline "~/capture/todo.org" "Tasks")
         "** TODO %?\n %i\n %a" :empty-lines 1)
        ("W" "Wishlist" plain
         (plain "~/capture/someday.org" "Wishlist")
         "** %^{thing}" :empty-lines 1)
        ("w" "Watch" entry
         (file+headline "~/capture/someday.org" "Watch")
         "** %^{movie title}\n %a" :empty-lines 1)))
;; List of Org-mode capture templates.

;;;; Export configuration

(require 'ox-latex)
(unless (boundp 'org-latex-classes)
  (setq org-latex-classes nil))
(add-to-list 'org-latex-classes
             '("article"
               "\\documentclass{article}"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
               ("\\paragraph{%s}" . "\\paragraph*{%s}")
               ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
(add-to-list 'org-latex-classes
             '("book"
               "\\documentclass{book}"
               ("\\part{%s}" . "\\part*{%s}")
               ("\\chapter{%s}" . "\\chapter*{%s}")
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")
               ("\\subsubsection{%s}" . "\\subsubsection*{%s}")))
(setq org-latex-listings 'minted          ; Use minted for code listings.
      org-latex-packages-alist '(("" "minted")) ; Include minted package.
      org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
;; LaTeX export settings.

(straight-use-package 'ox-hugo
  :config
  (setq-local org-hugo-base-dir "~/projects/web/hugo/"))
;; Hugo export backend for Org-mode.

(straight-use-package 'ox-mediawiki)
;; MediaWiki export backend for Org-mode.

(setq org-export-backends '(ascii html hugo latex md mediawiki slimhtml))
;; Enabled export backends.

;;;; Babel configuration

(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (calc . t)
   (clojure . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (haskell . t)
   (js . t)
   (latex . t)
   (lisp . t)
   (ocaml . t)
   (org . t)
   (perl . t)
   (plantuml . t)
   (python . t)
   (R . t)
   (ruby . t)
   (sass . t)
   (scheme . t)
   (shell . t)
   (sql . t)))
;; Load languages for Org Babel execution.

;;;; Calendar settings

(setq calendar-latitude -40.406925)      ; Latitude for calendar calculations.
(setq calendar-longitude 175.578386)     ; Longitude for calendar calculations.
(setq holiday-general-holidays nil)      ; Disable general holidays.
(setq holiday-christian-holidays nil)    ; Disable Christian holidays.
(setq holiday-hebrew-holidays nil)       ; Disable Hebrew holidays.
(setq holiday-islamic-holidays nil)      ; Disable Islamic holidays.
(setq holiday-bahai-holidays nil)        ; Disable Baháʼí holidays.
(setq holiday-oriental-holidays nil)     ; Disable Oriental holidays.


;;;; Finalization

(org-agenda-redo-all)                    ; Refresh all agenda views on load.

(provide 'core-orgmode)

;;; core-orgmode.el ends here
