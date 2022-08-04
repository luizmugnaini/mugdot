;;; init-rust.el --- rust config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rust-mode
  :hook (rust-mode . (lambda () (setq indent-tabs-mode nil)))
  :config
  (define-key rust-mode-map (kbd "C-c C-c") 'rust-run)
  :custom
  (rust-format-on-save t))

;; (add-hook 'rust-mode-hook
;;           (lambda () (setq indent-tabs-mode nil)))

(provide 'init-rust)
;;; init-rust.el ends here
