;;; init-font.el --- font config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar mug/default-font-size 140)
(defvar mug/default-variable-font-size 140)
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height mug/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font" :height mug/default-variable-font-size)

;; Font ligatures!
;; (use-package fira-code-mode
;;   :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
;;   :config (global-fira-code-mode)
;;   ;; Enables fira-code-mode automatically for programming major modes
  ;; :hook prog-mode)

(provide 'init-font)
;;; init-font.el ends here
