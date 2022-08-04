;;; init-evil.el --- evil config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun mug/evil-hook ()
  "My evil mode."
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

;; Sorry Emacs nerd, but Vim rules
(setq-default evil-want-keybinding nil)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  (setq evil-shift-width 2)
  (setq evil-insert-state-cursor 'box) ;; Cursor style at insert-mode
  :config
  (add-hook 'evil-mode-hook 'mug/evil-hook)
  (evil-mode 1)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

;; Nerd commenter for Emacs: toggle comment with M-/
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Allows the use of u for undos in evil mode
(use-package undo-tree)
(global-undo-tree-mode)

(provide 'init-evil)
;;; init-evil.el ends here
