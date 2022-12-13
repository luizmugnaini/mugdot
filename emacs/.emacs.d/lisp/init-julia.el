;;; init-julia.el --- julia config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package julia-mode
  :hook (julia-mode . lsp-deferred)
  :config
  (add-to-list 'load-path "/usr/bin/julia"))

(use-package julia-repl
  :hook (julia-mode . julia-repl-mode))

(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7"))

(use-package flycheck-julia)
(flycheck-julia-setup)

(provide 'init-julia)
;;; init-julia.el ends here
