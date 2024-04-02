;;; Code:


;; Configuration directory.
(setq emacs-directory "C:/Users/luizm/.config/mugdot/emacs")

;; Maps the location of the custom stuff to a better place.
(setq custom-file (concat emacs-directory "/custom.el"))

(setq emacs-cache "C:/Users/luizm/AppData/Local/emacs")

;; Change the user-emacs-directory to keep unwanted things out of the emacs directory.
(setq user-emacs-directory emacs-directory)

(setq projects-directory "D:/projects")

(provide 'paths)
;;; Local Variables:
;;; init.el ends here
