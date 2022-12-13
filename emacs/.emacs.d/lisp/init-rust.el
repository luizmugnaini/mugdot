;;; init-rust.el --- rust config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :hook ((rust-mode . lsp-deferred)
         (rust-mode . (lambda ()
                        (setq indent-tabs-mode nil))))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  (define-key rust-mode-map (kbd "C-c t") 'rust-test)
  (define-key rust-mode-map (kbd "C-c k") 'rust-check)
  :custom
  (rust-format-on-save t))

(provide 'init-rust)
;;; init-rust.el ends here
