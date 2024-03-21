;;; Emacs --- My Emacs configurations

;;; Commentary:
;; Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>

;;; Code:

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

(defun mug/display-startup-time ()
  "Display the Emacs startup time."
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'mug/display-startup-time)

(setq default-directory "d:/")
(add-to-list 'load-path "c:/Users/luizm/.config/mugdot/emacs")
(require 'paths)

;; Init code is contained in `lisp/'
(add-to-list 'load-path (concat emacs-directory "/lisp"))

(require 'init-package)

;;; General behaviour

(require 'init-font)
(require 'init-behave)
(require 'init-appearence)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-counsel)
(require 'init-utils)

;;; File and project management:

(require 'init-dired)
(require 'init-projectile)

;;; Writing:

(require 'init-latex)

;;; Programming:

(require 'init-git)

(require 'init-completion)
(require 'init-lsp)

(require 'init-rust)
(require 'init-python)

(provide 'init)
;;; Local Variables:
;;; no-byte-compile: t
;;; init.el ends here
