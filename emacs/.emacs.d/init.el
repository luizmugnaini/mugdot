;;; Emacs --- My Emacs configurations

;;; Commentary:
;; Luiz Mugnaini -> luizmugnaini@gmail.com

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

;; Init code is contained in `lisp/'
;; (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Maps the location of the custom stuff to a better place
(setq custom-file "~/.emacs.d/custom.el")

;; Packaging
(require 'init-package)

;; General behaviour, appearence and keybindings
(require 'init-font)
(require 'init-behave)
(require 'init-appearence)
(require 'init-keybindings)
(require 'init-evil)
(require 'init-counsel)

;; Terminal emulator
(require 'init-vterm)

;; Version control
(require 'init-git)

;; File and project management
(require 'init-dired)
(require 'init-projectile)

;; Completion
;; (require 'init-ivy)
(require 'init-completion)

;; Language support and such
(require 'init-lsp)

;;; Writing:

;; Latex
(require 'init-latex)

;; Org
(require 'init-org)

;;; Programming:

;; Rust
(require 'init-rust)

;; Python
(require 'init-python)

;; Julia
(require 'init-julia)

;; Haskell
;; (require 'init-haskell)

(provide 'init)
;;; Local Variables:
;;; no-byte-compile: t
;;; init.el ends here
