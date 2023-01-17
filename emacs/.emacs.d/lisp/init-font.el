;;; init-font.el --- font config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defvar mug/default-font-size 140)
(defvar mug/default-variable-font-size 140)
(defvar mug/small-screen-font-size 180)
(defvar mug/small-screen-variable-font-size 180)
(set-face-attribute 'default nil :font "Iosevka Nerd Font" :height mug/small-screen-font-size)
(set-face-attribute 'fixed-pitch nil :font "Iosevka Nerd Font" :height mug/small-screen-font-size)

(provide 'init-font)
;;; init-font.el ends here
