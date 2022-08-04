;;; init-dired.el --- dired config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;; File management

(use-package all-the-icons-dired
  :after dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package dired
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-listing-switches "-agho --group-directories-first")

  ;; Image previewing
  (setq image-dired-external-viewer "feh")
  (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
  (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external)
  (put 'dired-find-alternate-file 'disabled nil)

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" (lambda () (interactive) (find-alternate-file ".."))
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))

(provide 'init-dired)
;;; init-dired.el ends here
