;;; init-lsp.el --- lsp config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred)
	(haskell-mode . lsp-deferred)
	(rust-mode . lsp-deferred)
	(julia-mode . lsp-deferred)
	(lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-file-watchers nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package flycheck
  :straight t
  :init
  (global-flycheck-mode t))

(use-package company-tabnine
  :straight t)

(provide 'init-lsp)
;;; init-lsp.el ends here
