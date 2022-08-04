;;; early-init.el --- Emacs pre-inicialisation config
;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
