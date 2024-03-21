;;; init-font.el --- font config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar mug/default-font-size 100)
(defvar mug/default-variable-font-size 100)
(defvar mug/small-screen-font-size 140)
(defvar mug/small-screen-variable-font-size 140)
(set-face-attribute 'default nil :font "FiraCode Nerd Font" :height mug/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height mug/default-font-size)

(provide 'init-font)
;;; init-font.el ends here
