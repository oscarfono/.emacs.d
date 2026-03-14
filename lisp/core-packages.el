;;; core-packages.el --- Package management with straight.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Cooper Oscarfono
;;
;; Author: Cooper Oscarfono <cooper@oscarfono.com>
;; Maintainer: Cooper Oscarfono <cooper@oscarfono.com>
;; Created: March 19, 2025
;; Last Modified: March 14, 2026
;; Keywords: lisp, packages, configuration
;; Package-Requires: ((emacs "29.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Package installation and configuration using `straight.el' with `use-package'.
;; Runs on both macOS and Linux. Platform-specific branches are clearly marked.
;;
;; Startup optimisation strategy:
;;   - :defer t        load on first use
;;   - :commands       load when command is called (implies autoload)
;;   - :hook           load when hook fires (implies :defer t)
;;   - :bind           load when key is pressed (implies autoload)
;;   - :bind-keymap    creates a prefix key without loading the package
;;
;; Packages that MUST load eagerly (no deferral):
;;   - straight/use-package infrastructure
;;   - treesit config (sets variables, no package to load)
;;   - major-mode-remap-alist (must be set before any file opens)
;;   - company (global-company-mode needs to be active immediately)
;;   - helm (helm-mode must be active for M-x override to work)
;;
;; CHANGES (2026-03-14):
;;   - Deferred: magit, projectile, flycheck, undo-tree, docker,
;;     elfeed, org-brain, nix-mode, dockerfile-mode, docker-compose-mode,
;;     package-lint, sr-speedbar, multiple-cursors, aggressive-indent.
;;   - Replaced global-flycheck-mode with prog-mode hook (loads on demand).
;;   - multi-term: expanded config with shell, scrolling, dedicated window.

;;; Code:

;;;; ============================================================
;;;; Core utilities
;;;; ============================================================

(use-package delight)
;; Loaded eagerly — modeline changes need to be in effect from the start.

(use-package bug-hunter
  :defer t)

;; Only needed on macOS — Linux inherits shell env correctly.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(use-package multi-term
  :bind (("C-M-SPC" . multi-term)
         ("C-M-]"   . multi-term-next)
         ("C-M-["   . multi-term-prev)
         ("C-c t"   . multi-term-dedicated-toggle))
  :config
  ;; Use login shell so aliases, functions, and $PATH are present.
  (setq multi-term-program (or (getenv "SHELL") "/bin/bash"))
  (setq multi-term-scroll-to-bottom-on-output t)
  (setq multi-term-scroll-show-maximum-output t)
  ;; Tell programs the terminal supports 256 colours.
  ;; Without this, ls, git, grep etc. won't emit colour escape codes.
  (setenv "TERM" "xterm-256color")
  (setq multi-term-dedicated-window-height 20)
  ;; yasnippet intercepts TAB in term buffers — disable it there.
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

;;;; ============================================================
;;;; Editing enhancements
;;;; ============================================================

(use-package aggressive-indent
  :defer t
  :hook (prog-mode . aggressive-indent-mode)
  :config
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package undo-tree
  :defer t
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package smartparens
  :defer t
  :hook ((prog-mode     . turn-on-smartparens-strict-mode)
         (markdown-mode . turn-on-smartparens-strict-mode))
  :config
  (show-smartparens-global-mode t))

(use-package yasnippet
  :defer t
  :hook ((prog-mode . yas-minor-mode)
         (text-mode . yas-minor-mode))
  :config
  (yas-reload-all))

(use-package yasnippet-classic-snippets :defer t)
(use-package yasnippet-snippets         :defer t)

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("M-<" . company-select-first)
         ("M->" . company-select-last))
  :config
  (global-company-mode t))
;; Loaded eagerly — global-company-mode must be active from the start.
;; LSP backends take priority when lsp-mode is active.

(use-package company-ansible  :defer t)
(use-package company-c-headers :defer t)
(use-package company-ctags    :defer t)
(use-package company-nginx    :defer t)
(use-package company-shell    :defer t)
(use-package company-web      :defer t)

;;;; ============================================================
;;;; Org-mode enhancements
;;;; ============================================================

(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

(use-package org-brain
  :defer t
  :commands (org-brain-visualize org-brain-prefix-map)
  :init
  (setq org-brain-path "~/Documents/brain")
  :bind (:map org-mode-map
         ("C-c b" . org-brain-prefix-map))
  :config
  (setq org-id-track-globally t
        org-id-locations-file "~/.emacs.d/.org-id-locations")
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'org-brain-ensure-ids-in-buffer nil t))))

(use-package gnuplot
  :defer t
  :commands gnuplot-mode
  :bind ("C-M-g" . gnuplot))

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
;; Loaded eagerly — doom-modeline needs icons available at startup.

(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))

(use-package rainbow-mode
  :defer t
  :hook (prog-mode . rainbow-mode))

(use-package sr-speedbar
  :defer t
  :bind ("M-s" . sr-speedbar-toggle)
  :custom
  (sr-speedbar-right-side t)
  (speedbar-show-unknown-files t)
  (sr-speedbar-width 50)
  (sr-speedbar-max-width 35)
  :config
  (setq speedbar-use-images nil))

;;;; ============================================================
;;;; Project and version control
;;;; ============================================================

(use-package projectile
  :defer t
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package magit
  :defer t
  :bind ("C-x g" . magit-status))

;;;; ============================================================
;;;; Tree-sitter (built-in, Emacs 29+)
;;;;
;;;; First-time setup: M-x my/treesit-install-all-grammars
;;;; Requires gcc and git on PATH.
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

;; Level 4: operators, delimiters, variable references, property accesses.
(setq treesit-font-lock-level 4)

;; Remap legacy modes to ts-mode equivalents when a grammar is available.
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
  "Download and compile all grammars in `my/treesit-language-sources'.
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
  :defer t
  :hook (prog-mode . flycheck-mode))
;; Deferred: loads when first prog-mode buffer opens.
;; flycheck-mode per-buffer is equivalent to global-flycheck-mode in practice.

(use-package package-lint
  :defer t)

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  :config
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

(use-package nix-mode
  :defer t)

;; rustic: cargo, clippy, format-on-save, test running.
;; rustic-lsp-client calls lsp automatically — no manual (lsp) needed.
;; Requires: rustup component add rust-analyzer
(use-package rustic
  :defer t
  :bind (:map rustic-mode-map
         ("M-j" . lsp-ui-imenu)
         ("M-?" . lsp-find-references))
  :config
  (setq rustic-format-on-save t
        rustic-lsp-client 'lsp-mode)
  (add-hook 'rustic-mode-hook
            (lambda ()
              (when (treesit-ready-p 'rust t)
                (treesit-parser-create 'rust)))))

(use-package helm
  :bind (("C-c h" . helm-mini)
         ("M-x"   . helm-M-x))
  :config
  (setq helm-candidate-number-limit 100)
  (helm-mode))
;; Loaded eagerly via :bind autoload on M-x override.
;; helm-mode must activate before the user hits M-x.

(use-package helm-projectile
  :defer t
  :after (helm projectile)
  :config
  (helm-projectile-on))

(use-package docker
  :defer t
  :bind ("C-c d" . docker))

(use-package dockerfile-mode    :defer t)
(use-package docker-compose-mode :defer t)

;;;; ============================================================
;;;; Other tools
;;;; ============================================================

(use-package elfeed
  :defer t
  :bind ("C-x w" . elfeed)
  :config
  (setq elfeed-feeds '("http://nullprogram.com/feed/"
                       "https://sachachua.com/blog/category/emacs-news/feed/")))

(use-package transient
  :straight t
  :ensure t)
;; Loaded eagerly — aidermacs declares :after transient.

(use-package aidermacs
  :straight (:type git :host github :repo "MatthewZMD/aidermacs")
  :ensure t
  :after transient
  :bind ("C-x a" . aidermacs-transient-menu)
  :config
  (add-hook 'aidermacs-before-run-backend-hook
            (lambda ()
              (setenv "ANTHROPIC_API_KEY"
                      (auth-source-pick-first-password
                       :host "api.anthropic.com"
                       :user "cooper@oscarfono.com"))
              (setenv "GOOGLE_API_KEY"
                      (auth-source-pick-first-password
                       :host "api.google.com"
                       :user "cooper@adsono.live")))))

(provide 'core-packages)

;;; core-packages.el ends here
