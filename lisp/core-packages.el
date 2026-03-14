;;; core-packages.el --- Package management with straight.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 13, 2026
;; Keywords: lisp, packages, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Package installation and configuration using `straight.el' with `use-package'.
;;
;; BREAKING CHANGES (2026-03-13):
;;   - Requires Emacs 29.1+ (was 27.1) — built-in treesit is a 29+ feature.
;;   - Removed `tree-sitter' and `tree-sitter-langs' (third-party packages).
;;     Replaced with Emacs 29 built-in treesit.
;;     First-time setup: M-x my/treesit-install-all-grammars
;;     Requires gcc and git on PATH.
;;   - Removed `racer' — superseded by rust-analyzer via lsp-mode.
;;     Install: rustup component add rust-analyzer
;;   - Removed `rust-mode' — rustic supersedes it; both caused mode conflicts.
;;   - Removed `company-go' — gopls via lsp-mode handles Go completion.
;;   - Removed `js2-mode' — replaced by built-in js-ts-mode.
;;     Action required: rename any js2-mode-hook references to js-ts-mode-hook.
;;   - Fixed: `rk/rustic-mode-hook' was referenced but never defined anywhere
;;     in the config. Replaced with an inline hook.

;;; Code:

;;;; ============================================================
;;;; Core utilities
;;;; ============================================================

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

;;;; ============================================================
;;;; Editing enhancements
;;;; ============================================================

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
(use-package yasnippet-snippets)

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-<" . company-select-first)
         ("M->" . company-select-last))
  :config
  (global-company-mode t))
;; Completion framework. LSP backends take priority when lsp-mode is active.

(use-package company-ansible)
(use-package company-c-headers)
(use-package company-ctags)
(use-package company-nginx)
(use-package company-shell)
(use-package company-web)

;;;; ============================================================
;;;; Org-mode enhancements
;;;; ============================================================

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

;;;; ============================================================
;;;; UI and visuals
;;;; ============================================================

(use-package nerd-icons
  :straight (:type git :host github :repo "rainstormstudio/nerd-icons.el")
  :ensure t
  :if (display-graphic-p)
  :config
  (unless (member "Symbols Nerd Font Mono" (font-family-list))
    (nerd-icons-install-fonts t)))
;; Icon set for enhancing UI elements, including doom-modeline.

(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))
;; Fancy mode line using Nerd Fonts.

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

;;;; ============================================================
;;;; Project and version control
;;;; ============================================================

(use-package projectile
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))
;; Project management and navigation.

(use-package magit
  :bind ("C-x g" . magit-status))
;; Git interface for Emacs.

;;;; ============================================================
;;;; Tree-sitter (built-in, Emacs 29+)
;;;;
;;;; Replaces the third-party `tree-sitter' / `tree-sitter-langs' packages.
;;;; First-time setup: M-x my/treesit-install-all-grammars
;;;; Grammars install to: ~/.emacs.d/tree-sitter/
;;;; ============================================================

(defvar my/treesit-language-sources
  '((bash       . ("https://github.com/tree-sitter/tree-sitter-bash"))
    (c          . ("https://github.com/tree-sitter/tree-sitter-c"))
    (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp"))
    (css        . ("https://github.com/tree-sitter/tree-sitter-css"))
    (go         . ("https://github.com/tree-sitter/tree-sitter-go"))
    (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod"))
    (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
    (json       . ("https://github.com/tree-sitter/tree-sitter-json"))
    (python     . ("https://github.com/tree-sitter/tree-sitter-python"))
    (rust       . ("https://github.com/tree-sitter/tree-sitter-rust"))
    (toml       . ("https://github.com/tree-sitter/tree-sitter-toml"))
    (tsx        . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
    (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
    (yaml       . ("https://github.com/ikatyang/tree-sitter-yaml"))
    (zig        . ("https://github.com/maxxnino/tree-sitter-zig")))
  "Grammar sources used by `my/treesit-install-all-grammars'.")

(setq treesit-language-source-alist my/treesit-language-sources)

;; Level 4 enables full highlighting: operators, delimiters, variable
;; references, property accesses.  Default of 3 leaves these unfontified.
(setq treesit-font-lock-level 4)

;; Redirect legacy major modes to their ts-mode equivalents when a grammar
;; is available.  Emacs checks this alist automatically on file open.
(setq major-mode-remap-alist
      '((bash-mode       . bash-ts-mode)
        (c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (css-mode        . css-ts-mode)
        (go-mode         . go-ts-mode)
        (js-mode         . js-ts-mode)
        (json-mode       . json-ts-mode)
        (python-mode     . python-ts-mode)
        (rust-mode       . rust-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (yaml-mode       . yaml-ts-mode)))

(defun my/treesit-install-all-grammars ()
  "Download and compile all grammars listed in `my/treesit-language-sources'.
Run once after initial setup, or when adding new languages.
Requires gcc and git on PATH."
  (interactive)
  (mapc #'treesit-install-language-grammar
        (mapcar #'car my/treesit-language-sources))
  (message "treesit: grammars installed to %s"
           (expand-file-name "tree-sitter" user-emacs-directory)))

;;;; ============================================================
;;;; Language tools
;;;; ============================================================

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
;; UI enhancements for lsp-mode.

(use-package nix-mode)
;; Major mode for Nix expressions.

;; rustic provides Rust editing with cargo, clippy, format-on-save, and test
;; running.  rust-mode and racer are not needed alongside it.
;; Ensure rust-analyzer is installed: rustup component add rust-analyzer
(use-package rustic
  :bind (:map rustic-mode-map
         ("M-j" . lsp-ui-imenu)
         ("M-?" . lsp-find-references))
  :config
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (lsp)
              ;; Provide the rust grammar for treesit font-lock in rustic buffers.
              (when (treesit-ready-p 'rust t)
                (treesit-parser-create 'rust)))))

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
(use-package docker-compose-mode)

;;;; ============================================================
;;;; Other tools
;;;; ============================================================

(use-package elfeed
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds '("http://nullprogram.com/feed/"
                       "https://sachachua.com/blog/category/emacs-news/feed/")))
;; RSS and Atom feed reader.

(use-package transient
  :straight t
  :ensure t)
;; Required by aidermacs.

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
;; AI coding assistant via aider.

(provide 'core-packages)

;;; core-packages.el ends here
