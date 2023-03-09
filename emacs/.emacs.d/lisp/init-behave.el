;;; init-behave.el --- behaviour config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Buffer management ----------------------------------------------------------
;; Split windows vertically by default (maybe I'll regret this)
(split-window-right)

(use-package popwin
  :init
  (popwin-mode 1)
  :config
  (global-set-key (kbd "C-w") popwin:keymap)
  (setq popwin:popup-window-height 10)

  ;; Deal with flycheck exploding in your face while coding
  (push '(flycheck-error-list-mode :height 7) popwin:special-display-config)
  (push '(flycheck-error-message-mode :height 7) popwin:special-display-config)
  (push '(lsp-help-mode :height 7) popwin:special-display-config)
  (push '(lsp-diagnostics-mode :height 7) popwin:special-display-config))

(defun bury-compile-buffer-if-successful (buffer string)
 "Bury a compilation buffer if succeeded without warnings "
 (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (run-with-timer 1 nil
                    (lambda (buf)
                      (bury-buffer buf)
                      (switch-to-prev-buffer (get-buffer-window buf) 'kill))
                    buffer)))
(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

(use-package transpose-frame)

;;; Text position on the screen ------------------------------------------------

(defun mug/prog-mode-visual-fill ()
  "Side columns filling."
  (setq visual-fill-column-width 101
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun mug/latex-mode-visual-fill ()
  "Side columns filling."
  (setq visual-fill-column-width 90
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook ((prog-mode  . mug/prog-mode-visual-fill)
         (LaTeX-mode . mug/latex-mode-visual-fill)
         (markdown-mode . mug/prog-mode-visual-fill)))

;; Wrapping lines with > 80 chars
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
;; (add-hook 'prog-mode-hook #'auto-fill-mode)

;;; Line numbers and stuff -----------------------------------------------------
;; The current behaviour is to not display line numbers, since I find them more
;; annoying than helpful on a daily basis.

;; Shows the column number in the modeline
(column-number-mode)

;; Configurations for programming modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                LaTeX-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))

;; Other modes
(dolist (mode '(org-mode-hook
	      	term-mode-hook
		shell-mode-hook
                dired-mode-hook
		eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))

;;; Filesystem backups ---------------------------------------------------------

(setq make-backup-files nil ;; disable backup
      create-lockfiles nil)

;; Use no-littering to automatically set common paths to the new
;; user-emacs-directory
(use-package no-littering)

;;; .emacs.d dir ---------------------------------------------------------------

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;;; Emacs behaviour: -----------------------------------------------------------

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

;; Window sizing
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(provide 'init-behave)
;;; init-behave.el ends here
