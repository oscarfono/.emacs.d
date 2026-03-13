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
;; and keybindings.  It is designed to work with the latest Org-mode version
;; installed via `straight.el' from `early-init.el'.

;;; Code:

;;;; Global settings

(setq org-startup-folded t)             ; Collapse headlines on file open.
(setq org-startup-align-all-tables t)    ; Align tables when opening files.
(setq org-clock-persist 'history)        ; Persist clock history across sessions.
(org-clock-persistence-insinuate)        ; Enable clock persistence mechanism.

;;;; TODOs and priorities

(setq org-agenda-files (list "~/Capture/todo.org"
                             "~/Capture/contacts.org"))  ; Files for agenda tracking.
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
;; All skeleton bindings live under the C-c s prefix (s for skeleton).
;; The differentiating letter matches the first letter of the skeleton
;; name after "core-orgmode-": w=web, b=blog, o=org, a=apa, l=letter.
;; C-c s is unbound in both stock Emacs and org-mode, making it safe
;; for user-defined sequences.

(define-prefix-command 'core-orgmode-skeleton-map)
(global-set-key (kbd "C-c s") 'core-orgmode-skeleton-map)

(define-key core-orgmode-skeleton-map (kbd "w") #'core-orgmode-web-skeleton)
(define-key core-orgmode-skeleton-map (kbd "b") #'core-orgmode-blog-skeleton)
(define-key core-orgmode-skeleton-map (kbd "o") #'core-orgmode-org-skeleton)
(define-key core-orgmode-skeleton-map (kbd "a") #'core-orgmode-apa-skeleton)
(define-key core-orgmode-skeleton-map (kbd "l") #'core-orgmode-ltr-skeleton)

;;;; Capture templates

(defvar core-orgmode-contacts-template
  "** %^{contact}
    :PROPERTIES:
      :EMAIL: %(org-contacts-template-email)
      :PHONE: %^{XXX-XXX-XXXX}
      :ADDRESS: %^{street name. city, state, postcode }
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
      `(("b" "Birthday" entry
         (file+headline "~/Capture/contacts.org" "Birthdays")
         "* %^{Name}'s Birthday\n  %^{Date of birth}T\n  :PROPERTIES:\n  :CATEGORY: birthday\n  :END:\n"
         :empty-lines 1)
        ("c" "Contact" entry
         (file+headline "~/Capture/contacts.org" "Contacts")
         ,core-orgmode-contacts-template :empty-lines 1)
        ("d" "Documentation" entry
         (file+headline "~/Documents/docs.org" "Documentation")
         "** %^{Subject}\n %^g\n %?\n %i\n Added %U")
        ("D" "Definition" entry
         (file+headline "~/Capture/definitions.org" "Definitions")
         "** %^{Term} :: %^{Definition} ")
        ("e" "Expense" entry
         (file+olp+datetree "~/Capture/expenses.org")
         ,core-orgmode-expenses-template :empty-lines 1)
        ("i" "Idea" entry
         (file+olp+datetree "~/Capture/ideas.org" "Ideas")
         "** \ue910 %?\n I had this idea on %U\n %a" :empty-lines 1)
        ("j" "Journal" entry
         (file+olp+datetree "~/Capture/journal.org")
         "* \ue916 %?\n Entered on %U\n" :empty-lines 1)
        ("l" "Lyric" entry
         (file+headline "~/Capture/lyrical-ideas.org" "Lyrical Ideas Capture")
         "** %^{working-title}\n %^{verse}\n %^{hook}\n")
        ("p" "Project" entry
         (file+olp+datetree "~/Capture/current-projects.org")
         ,core-orgmode-project-template :empty-lines 1)
        ("Q" "Quote" entry
         (plain "~/Capture/quotes.org")
         ,core-orgmode-greatquotes-template :empty-lines 1)
        ("r" "Read" entry
         (file+headline "~/Capture/someday.org" "Read")
         "** %^{title}\n %^{author}" :empty-lines 1)
        ("s" "Subject" entry
         (file+headline "~/Capture/someday.org" "Write")
         "** %^{subject}\n" :empty-lines 1)
        ("t" "Todo" entry
         (file+headline "~/Capture/todo.org" "Tasks")
         "** TODO %?\n %i\n %a" :empty-lines 1)
        ("W" "Wishlist" plain
         (plain "~/Capture/someday.org" "Wishlist")
         "** %^{thing}" :empty-lines 1)
        ("w" "Watch" entry
         (file+headline "~/Capture/someday.org" "Watch")
         "** %^{movie title}\n %a" :empty-lines 1)))

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
(add-to-list 'org-latex-classes
             '("letter"
               "\\documentclass{letter}
                \\usepackage[margin=1in]{geometry}
                [NO-DEFAULT-PACKAGES]
                [NO-PACKAGES]"
               ("\\section{%s}" . "\\section*{%s}")
               ("\\subsection{%s}" . "\\subsection*{%s}")))

(setq org-latex-listings 'minted          ; Use minted for code listings.
      org-latex-packages-alist '(("" "minted")) ; Include minted package.
      org-latex-pdf-process '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Hugo export setup
(straight-use-package 'ox-hugo)
(with-eval-after-load 'ox
  (require 'ox-hugo))
(setq org-hugo-base-dir "~/Projects")

(straight-use-package 'ox-mediawiki)

(setq org-export-backends '(ascii html hugo latex md mediawiki slimhtml))

;;;; Babel configuration

(setq org-plantuml-jar-path
      (let ((local-path (expand-file-name "~/.emacs.d/lib/plantuml.jar" default-directory))
            (global-path "/usr/local/share/plantuml/plantuml.jar"))
        (if (file-exists-p local-path)
            local-path
          global-path)))

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

;;;; Handle variable and fixed pitch properly
;;;; Handle variable and fixed pitch properly
(use-package mixed-pitch
  :hook (org-mode . mixed-pitch-mode)
  :config
  ;; Ensure all fixed-width org faces stay monospace under mixed-pitch-mode
  (dolist (face '(org-table
                  org-block
                  org-block-begin-line
                  org-block-end-line
                  org-code
                  org-verbatim
                  org-formula
                  org-date
                  org-special-keyword
                  org-priority
                  org-tag))
    (add-to-list 'mixed-pitch-fixed-pitch-faces face)))

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

(with-eval-after-load 'org-agenda
  (when (get-buffer "*Org Agenda*")
    (with-current-buffer "*Org Agenda*"
      (org-agenda-redo-all))))

(provide 'core-orgmode)

;;; core-orgmode.el ends here
