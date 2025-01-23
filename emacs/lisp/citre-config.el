;;; citre-config.el --- Custom config for Citre -*- lexical-binding: t -*-

;;; Commentary:

;; This is my custom config for the Citre package. It enables all language support
;; that Citre provides, and autoloads `citre' on some commands that's not
;; define in citre.el.
;;
;; You should put (require 'citre-config) in the `:init' block of the `citre'.

;;; Code:

;;;; Auto enabling `citre-mode'

;; This is autoloaded in citre.el so it's usable.
(declare-function citre-auto-enable-citre-mode "citre")
(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)

;;;; Language supports

(with-eval-after-load 'simpc-mode (require 'citre-lang-c))
(with-eval-after-load 'dired (require 'citre-lang-fileref))

;;;; Autoload

(autoload 'citre-update-tags-file "citre" nil t)
(autoload 'citre-update-this-tags-file "citre" nil t)
(autoload 'citre-edit-tags-file-recipe "citre" nil t)
(autoload 'citre-create-tags-file "citre" nil t)
(autoload 'citre-global-create-database "citre" nil t)
(autoload 'citre-global-update-database "citre" nil t)

(provide 'citre-config)

;;; citre-config.el ends here
