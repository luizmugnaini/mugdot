;;; init-lsp.el --- lsp config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-enable-file-watchers nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map (kbd "C-l d") 'lsp-ui-doc-toggle)
  (define-key lsp-ui-mode-map (kbd "C-l f") 'lsp-ui-doc-focus-frame)

  (setq lsp-ui-doc-max-height 15)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-sideline-show-diagnostics nil)
  (setq lsp-ui-sideline-show-code-actions nil))

;; Had to use global key here since I cannot figure out why this does not work
;; simply using the `lsp-ui-doc-mode-map'...
;; (global-set-key (kbd "C-l u") 'lsp-ui-doc-unfocus-frame)

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode t)
  :config
  ;; Getting rid of the ugly fringes
  (setq fringe-styles "no-fringes")

  ;; Remove annoying underline
  (setq flycheck-highlighting-mode nil))

(provide 'init-lsp)
;;; init-lsp.el ends here
