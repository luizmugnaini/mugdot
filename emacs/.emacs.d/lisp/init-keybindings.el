;;; init-keybindings.el --- keybingings config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key key-translation-map (kbd "C-k") (kbd "<escape>"))

;; Moves through buffers with C-M-j
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Don't fuck up my flow --- problems using ctrl and esc for the same key
(global-unset-key (kbd "C-h h"))
(global-unset-key (kbd "C-h k"))
(global-unset-key (kbd "C-h j"))
(global-unset-key (kbd "C-h l"))

;; always fucking pressing this shit and suspending my gui session
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;; Fucked-up solution to my emacs pinky:
;; This is needed for me because my keyboard uses the Caps Lock as an Fn key
;; when pressed together with some other key, in order to prevent such a fucking
;; bullshit, I translated every possible shit that could happen into the right
;; thing that it should be binded to in the first place.
(define-key key-translation-map (kbd "<up>") (kbd "w"))
(define-key key-translation-map (kbd "C-<up>") (kbd "C-w"))
(define-key key-translation-map (kbd "<left>") (kbd "a"))
(define-key key-translation-map (kbd "C-<left>") (kbd "C-a"))
(define-key key-translation-map (kbd "<down>") (kbd "s"))
(define-key key-translation-map (kbd "C-<down>") (kbd "C-s"))
(define-key key-translation-map (kbd "<right>") (kbd "d"))
(define-key key-translation-map (kbd "C-<right>") (kbd "C-d"))
(define-key key-translation-map (kbd "C-<print>") (kbd "C-p"))

(use-package crux
  :config
  (global-set-key (kbd "C-c o") 'crux-open-with)
  (global-set-key (kbd "C-c I") 'crux-find-user-init-file)
  (global-set-key (kbd "C-c k") 'crux-kill-other-buffers))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (local-set-key (kbd "C-c e") 'eval-buffer)))

;; Tells you the possible keys for certain commands
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.7))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
