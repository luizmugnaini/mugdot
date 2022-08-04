;;; init-org.el --- org config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun mug/org-mode-setup ()
  "Orgmode setup."
  (org-indent-mode)
  (variable-pitch-mode 0) ;; Fira is horrible with this thing on
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . mug/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun mug/org-mode-visual-fill ()
  "Side columns filling to make org mode more presentable."
  (setq visual-fill-column-width 80
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mug/org-mode-visual-fill))

(provide 'init-org)
;;; init-org.el ends here
