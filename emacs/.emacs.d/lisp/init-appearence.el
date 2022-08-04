;;; init-appearence.el --- appearence config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install icons for the themes
;; On first time run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :straight t)

;; A better mode ooline from Doom
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-challenger-deep t)) ;; last: palenight

;; Better delimiter coloring for emacs
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sintax highlighting
(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(provide 'init-appearence)
;;; init-appearence.el ends here
