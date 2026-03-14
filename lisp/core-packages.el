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
;; Runs on both macOS and Linux. Platform-specific branches are clearly marked.
;;
;; CHANGES (2026-03-13):
;;   - Removed tree-sitter/tree-sitter-langs; replaced with built-in treesit.
;;   - Removed racer (superseded by rust-analyzer), rust-mode (rustic supersedes
;;     it), company-go (gopls via lsp-mode), js2-mode (replaced by js-ts-mode).
;;   - Fixed rk/rustic-mode-hook (was undefined); replaced with inline hook.
;;   - Fixed rustic double-lsp invocation: let rustic-lsp-client handle it.
;;   - Fixed rainbow-mode: was called at top level, now hooked to prog-mode.
;;   - Fixed org-brain before-save-hook: scoped to org-mode buffers only.
;;   - exec-path-from-shell now only runs on macOS (not needed on Linux).

;;; Code:

;;;; ============================================================
;;;; Core utilities
;;;; ============================================================

(use-package delight)
;; Diminish minor modes in the mode line.

(use-package bug-hunter)
;; Tool for debugging Emacs configuration issues.

;; exec-path-from-shell is only needed on macOS, where GUI Emacs doesn't
;; inherit the shell PATH. On Linux the environment is inherited correctly.
(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :config
  (exec-path-from-shell-initialize))

(use-package multi-term
  :bind (("C-M-SPC" . multi-term)
         ("C-M-]"   . multi-term-next)
         ("C-M-["   . multi-term-prev))
  :config
  ;; Use the login shell so aliases, functions, and $PATH are all present.
  (setq multi-term-program (or (getenv "SHELL") "/bin/bash"))

  ;; Keep the terminal scrolled to the latest output.
  (setq multi-term-scroll-to-bottom-on-output t)
  (setq multi-term-scroll-show-maximum-output t)

  ;; Dedicated terminal window at the bottom — toggle with C-c t.
  (global-set-key (kbd "C-c t") #'multi-term-dedicated-toggle)
  (setq multi-term-dedicated-window-height 20)

  ;; Don't let yasnippet expand in term buffers — it intercepts TAB.
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))
;; Multiple terminal emulator for Emacs.

;;;; ============================================================
;;;; Editing enhancements
;;;; ============================================================

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (global-undo-tree-mode))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  :config
  (define-key mc/keymap (kbd "<return>") nil))

(use-package smartparens
  :config
  (show-smartparens-global-mode t)
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook #'turn-on-smartparens-strict-mode))

(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))

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
;; LSP backends take priority over static backends when lsp-mode is active.

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

(use-package org-brain
  :init
  (setq org-brain-path "~/Documents/brain")
  :bind (:map org-mode-map
         ("C-c b" . org-brain-prefix-map))
  :config
  (setq org-id-track-globally t
        org-id-locations-file "~/.emacs.d/.org-id-locations")
  ;; Scoped to org buffers only — not every buffer on every save.
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'org-brain-ensure-ids-in-buffer nil t))))

(use-package gnuplot
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

(use-package doom-modeline
  :straight (:type git :host github :repo "seagle0128/doom-modeline")
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project))

;; rainbow-mode is buffer-local — hook it, don't call it at top level.
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package sr-speedbar
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
  :config
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package magit
  :bind ("C-x g" . magit-status))

;;;; ============================================================
;;;; Tree-sitter (built-in, Emacs 29+)
;;;;
;;;; First-time setup: M-x my/treesit-install-all-grammars
;;;; Requires gcc and git on PATH (both standard on macOS with Xcode CLT
;;;; and on Linux).  Grammars install to: ~/.emacs.d/tree-sitter/
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

;; Level 4: full highlighting — operators, delimiters, variable references,
;; property accesses.  Default of 3 leaves these unfontified.
(setq treesit-font-lock-level 4)

;; Redirect legacy major modes to ts-mode equivalents when a grammar exists.
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
  :config
  (global-flycheck-mode))

(use-package package-lint)

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

(use-package nix-mode)

;; rustic handles cargo, clippy, format-on-save, and test running.
;; rustic-lsp-client defaults to 'lsp-mode, which calls lsp automatically —
;; no need for a manual (lsp) call in the hook.
;; Requires: rustup component add rust-analyzer
(use-package rustic
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
  :config
  (setq helm-candidate-number-limit 100)
  (helm-mode)
  :bind (("C-c h" . helm-mini)
         ("M-x" . helm-M-x)))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package docker
  :bind ("C-c d" . docker))

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
                      (auth-source-pick-first-password
                       :host "api.anthropic.com"
                       :user "cooper@oscarfono.com"))
              (setenv "GOOGLE_API_KEY"
                      (auth-source-pick-first-password
                       :host "api.google.com"
                       :user "cooper@adsono.live")))))

(provide 'core-packages)

;;; core-packages.el ends here
