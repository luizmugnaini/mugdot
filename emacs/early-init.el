;;; early-init.el --- Emacs pre-inicialisation config
;;; Commentary:

;; Emacs 27+ loads this file before (normally) calling
;; `package-initialize'.  We use this file to suppress that automatic
;; behaviour so that startup is consistent across Emacs versions.

;;; Code:

(setq package-enable-at-startup nil)

;; Avoid garbage collection at startup.
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

;; HACK: Avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters
;;   can trigger/queue a superfluous frame redraw at startup.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode             nil
      tool-bar-mode             nil
      scroll-bar-mode           nil
      tooltip-mode               -1
      column-number-mode         -1
      display-line-numbers-mode  -1)

;; Start with a black screen to prevent white flashing when opening emacs.
(add-to-list 'default-frame-alist '(background-color . "black"))

;; Startup with a fullscreen.
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; Don't resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

(provide 'early-init)
;;; early-init.el ends here
