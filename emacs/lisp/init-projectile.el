;;; init-projectile.el --- projectile config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :demand t
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ;; Every projectile command starts with C-c p
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'init-projectile)
;;; init-projectile.el ends here
