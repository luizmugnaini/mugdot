;;; init-python.el --- python config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package python
  :straight t
  :hook (python-mode . lsp-deferred)
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  (setq
    python-shell-interpreter "ipython"
    python-shell-interpreter-args "--profile=mugipy -i --simple-prompt"
    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
    python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"))

(use-package lsp-pyright
  :straight t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :config
  (setq lsp-pyright-venv-path "/home/mug/.cache/pypoetry/virtualenvs")
  (setq lsp-pyright-multi-root nil))

(use-package python-black
  :straight t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

(use-package poetry
  :straight t)

(provide 'init-python)
;;; init-python.el ends here
