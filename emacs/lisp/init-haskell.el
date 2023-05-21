;;; init-haskell.el --- haskell config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package haskell-mode)

(setq haskell-process-type 'ghci)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

(custom-set-variables '(haskell-process-type 'stack-ghci))

(use-package lsp-haskell
  :straight t
  :hook (haskell-mode . lsp)
  :custom
  (lsp-haskell-process-path-hie "cabal repl")
  (lsp-haskell-process-args-hie '())
  (lsp-log-io t))
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package haskell-snippets
  :straight t
  :after (haskell-mode yasnippet)
  :defer)

(provide 'init-haskell)
;;; init-haskell.el ends here
