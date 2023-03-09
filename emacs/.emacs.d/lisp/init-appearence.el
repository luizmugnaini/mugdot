;;; init-appearence.el --- appearence config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Install icons for the themes
;; On first time run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :straight t)

(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font
     t 'symbol (font-spec :family "Apple Color Emoji") nil 'prepend))
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))) ; override binding in any mode

(use-package page-break-lines)

(use-package dashboard
  :straight t
  :config
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-startup-banner "~/.emacs.d/assets/doom.png")
  (setq dashboard-banner-logo-title "Howdy mate! 🤠")
  (dashboard-setup-startup-hook))

;; A better mode line from Doom
(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil))

;; Please Emacs just stop asking if the theme is safe...
(setq custom-safe-themes t)
(use-package challenger-deep-theme)
(use-package base16-theme
  :init (load-theme 'base16-gruvbox-material-dark-hard))

;; Better delimiter coloring for emacs
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sintax highlighting
(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Transparency settings
(set-frame-parameter (selected-frame) 'alpha '(95 . 95))
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(provide 'init-appearence)
;;; init-appearence.el ends here
