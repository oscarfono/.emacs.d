;;; core-packages.el --- Package management with straight.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Keywords: lisp, packages, configuration
;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; This file manages package installation and configuration using `straight.el'
;; integrated with `use-package'.  It organizes packages into categories such as
;; core utilities, editing enhancements, Org-mode tools, UI improvements,
;; project management, language tools, and miscellaneous utilities.

;;; Code:

;;;; Core utilities

(use-package delight)
;; Diminish minor modes in the mode line.

(use-package bug-hunter)
;; Tool for debugging Emacs configuration issues.

(use-package exec-path-from-shell
  :init
  (autoload 'exec-path-from-shell-initialize "exec-path-from-shell" nil t)
  :config
  (add-hook 'after-init-hook #'exec-path-from-shell-initialize))
;; Synchronize shell environment variables with Emacs.

(use-package multi-term
  :bind (("C-M-SPC" . multi-term)))
;; Multiple terminal emulator for Emacs.

;;;; Editing enhancements

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))
;; Automatically indent code aggressively, excluding HTML mode.

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))
;; Enhanced undo system with a tree visualization.

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (define-key mc/keymap (kbd "<return>") nil))
;; Support for multiple cursors in editing.

(use-package smartparens
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-strict-mode))
;; Smart handling of parentheses and other paired delimiters.

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))
;; Template system for inserting snippets.

(use-package yasnippet-classic-snippets)
;; Classic snippet collection for yasnippet.

(use-package yasnippet-snippets)
;; Additional snippets for yasnippet.

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-<" . company-select-first)
         ("M->" . company-select-last))
  :config
  (global-company-mode t))
;; Completion framework with pluggable backends.

(use-package company-ansible)
;; Ansible completion backend.

(use-package company-c-headers)
;; C header completion.

(use-package company-ctags)
;; Ctags-based completion.

(use-package company-go)
;; Go language completion.

(use-package company-nginx)
;; Nginx completion.

(use-package company-shell)
;; Shell script completion.

(use-package company-web)
;; Web development completion.

;;;; Org-mode enhancements

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
;; Display Org-mode bullets as UTF-8 characters.

(use-package org-brain
  :init
  (setq org-brain-path "~/Documents/brain")
  :config
  (bind-key "C-c b" #'org-brain-prefix-map org-mode-map)
  (setq org-id-track-globally t
        org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'before-save-hook #'org-brain-ensure-ids-in-buffer))
;; Mind-mapping and note-taking with Org-mode.

(use-package gnuplot
  :commands gnuplot-mode
  :bind ("C-M-g" . gnuplot))
;; Interface to gnuplot for plotting.

;;;; UI and visuals

(use-package nerd-icons
  :straight (:type git :host github :repo "rainstormza/nerd-icons.el")
  :ensure t
  :if (display-graphic-p)
  :config
  ;; Install Nerd Fonts if not already present
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t)))  ; 't' skips confirmation
;; Icon set for enhancing UI elements, including doom-modeline.

(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t          ; Enable icons
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))
;; A fancy mode line inspired by Doom Emacs, using Nerd Fonts.

(use-package rainbow-mode
  :config
  (rainbow-mode))
;; Highlight color codes with their actual colors.

(use-package sr-speedbar
  :bind ("M-s" . sr-speedbar-toggle)
  :custom
  (sr-speedbar-right-side t)
  (speedbar-show-unknown-files t)
  (sr-speedbar-width 50)
  (sr-speedbar-max-width 35)
  :config
  (setq speedbar-use-images nil))
;; Speedbar integrated into a sidebar.

;;;; Project and version control

(use-package projectile
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))
;; Project management and navigation.

(use-package magit
  :bind ("C-x g" . magit-status))
;; Git interface for Emacs.

;;;; Language tools

(use-package flycheck
  :config
  (global-flycheck-mode))
;; On-the-fly syntax checking.

(use-package package-lint)
;; Linting for Emacs package metadata.

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))
;; Language Server Protocol support.

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))
;; UI enhancements for LSP-mode.

(use-package rust-mode)
;; Major mode for Rust programming.

(use-package rustic
  :bind (:map rustic-mode-map
         ("M-j" . lsp-ui-imenu)
         ("M-?" . lsp-find-references))
  :config
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook #'rk/rustic-mode-hook))
;; Enhanced Rust mode with LSP integration.

(use-package racer
  :config
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'rust-mode-hook #'eldoc-mode)
  (add-hook 'rust-mode-hook #'company-mode))
;; Rust code completion and navigation.

(use-package tree-sitter
  :straight (:type git :host github :repo "emacs-tree-sitter/elisp-tree-sitter")
  :ensure t
  :config
  (unless (member '(rustic-mode . rust) tree-sitter-major-mode-language-alist)
    (setq tree-sitter-major-mode-language-alist
          (cons '(rustic-mode . rust) tree-sitter-major-mode-language-alist))))
;; Syntax tree parsing with Tree-sitter.

(use-package tree-sitter-langs
  :straight (:type git :host github :repo "emacs-tree-sitter/elisp-tree-sitter"
             :files ("langs/*.el" "langs/queries"))
  :after tree-sitter)
;; Language definitions for Tree-sitter.

(use-package helm
  :config
  (setq helm-candidate-number-limit 100)
  (helm-mode)
  :bind (("C-c h" . helm-mini)
         ("M-x" . helm-M-x)))
;; Incremental completion and selection framework.

(use-package helm-projectile
  :config
  (helm-projectile-on))
;; Helm integration with Projectile.

(use-package docker
  :bind ("C-c d" . docker))
;; Docker management interface.

(use-package dockerfile-mode)
;; Mode for editing Dockerfiles.

(use-package docker-compose-mode)
;; Mode for editing Docker Compose files.

;;;; Other tools

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds '("http://nullprogram.com/feed/"
                       "https://sachachua.com/blog/category/emacs-news/feed/")))
;; RSS and Atom feed reader.

;; Ensure transient is available for aidermacs
(use-package transient
  :straight t
  :ensure t)

(use-package aidermacs
  :straight (:type git :host github :repo "MatthewZMD/aidermacs")
  :ensure t
  :after transient
  :bind ("C-x a" . aidermacs-transient-menu)
  :config
  (add-hook 'aidermacs-before-run-backend-hook
            (lambda ()
              (setenv "ANTHROPIC_API_KEY"
                      (auth-source-pick-first-password :host "api.anthropic.com"
                                                       :user "cooper@oscarfono.com"))
              (setenv "GOOGLE_API_KEY"
                      (auth-source-pick-first-password :host "api.google.com"
                                                       :user "cooper@adsono.live")))))
;; AI integration for Emacs.

(provide 'core-packages)

;;; core-packages.el ends here
