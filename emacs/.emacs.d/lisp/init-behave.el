;;; init-behave.el --- behaviour config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Line numbers:
(column-number-mode)
(global-display-line-numbers-mode t)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some other modes
(dolist (mode '(org-mode-hook
	      	term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Wrapping lines with > 80 chars
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)

;;; Filesystem backups ----------------------------------------------------------

(setq
 make-backup-files nil ; disable backup
 ;; auto-save-default nil
 create-lockfiles nil)

;; Use no-littering to automatically set common paths to the new
;; user-emacs-directory
(use-package no-littering)


;;; .emacs.d dir ----------------------------------------------------------------

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;;; Emacs behaviour: ------------------------------------------------------------

;; Deal with whitespaces
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Never use tab characters!
(setq-default indent-tabs-mode nil)

;; Prompt for y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Emacs look:
(setq inhibit-startup-message t)
(menu-bar-mode -1)

;; Inhibit ugly top of the window stuff
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Stops recentering of the frame while scroling
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Copy and paste from anywhere
(use-package xclip)
(xclip-mode 1)

;; Transparency settings
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package golden-ratio)
(golden-ratio-mode 1)

(provide 'init-behave)
;;; init-behave.el ends here
