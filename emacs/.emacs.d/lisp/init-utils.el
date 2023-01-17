;;; init-utils.el --- utilities config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rg)

(use-package fzf
  :bind ("C-c f" . fzf-projectile)
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "rg --no-heading -nH"
        ;; If nil, the fzf buffer will appear at the top of the window
        fzf/position-bottom t
        fzf/window-height 15))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (define-key hl-todo-mode-map (kbd "C-c C-t p") 'hl-todo-previous)
  (define-key hl-todo-mode-map (kbd "C-c C-t n") 'hl-todo-next)
  (define-key hl-todo-mode-map (kbd "C-c C-t o") 'hl-todo-occur)
  (define-key hl-todo-mode-map (kbd "C-c C-t i") 'hl-todo-insert)

  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

(provide 'init-utils)
;;; init-utils.el ends here
