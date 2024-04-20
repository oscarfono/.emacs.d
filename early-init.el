;;;; early-init.el --- early init file configuration for the emacs text editor, parsed before the init file or any customisations.
;;

;;; Commentary:
;; This is parsed before init.el

;;; Code:
;;
;; I disable package-enable-at-startup, prior to loading my init file, as package.el is no longer used, in favour of straight.el.
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here.
