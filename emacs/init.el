;;; Emacs --- init file --- -*- lexical-binding: t; -*-
;;; Commentary:
;; My custom init file for GNU Emacs.
;; Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>

;;; Code:

;;; Startup:

(defconst mug/emacs-start-time (current-time))

;; Avoid garbage collection at startup.
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

(defun mug/display-startup-time ()
  "Display the Emacs startup time."
  (message "Emacs loaded in %s with %d garbage collections."
	    (format "%.2f seconds"
		    (float-time
		    (time-subtract (current-time) mug/emacs-start-time)))
	    gcs-done))

(add-hook 'after-init-hook #'mug/display-startup-time) ; Report startup time
(add-hook 'after-init-hook #'garbage-collect t)        ; Restart garbage collection after startup

;;; Helper functions:

(defun mug/win-or-linux (win-option linux-option)
  "Choose between WIN-OPTION and LINUX-OPTION options."
  (if (eq system-type 'windows-nt)
      win-option
    linux-option))

(defun mug-c/find-impl-or-decl ()
  "Find the impl/header of the current buffer."
  (interactive)
  (let ((corresponding-file nil)
	(base-file-name (file-name-sans-extension buffer-file-name)))
    (cond
     ((string-match "\\.c" buffer-file-name)
      (setq corresponding-file (concat base-file-name ".h")))
     ((string-match "\\.cc" buffer-file-name)
      (setq corresponding-file (concat base-file-name ".h")))
     ((string-match "\\.cpp" buffer-file-name)
      (setq corresponding-file (concat base-file-name ".h")))
     ((string-match "\\.h" buffer-file-name)
      (let ((c-case (concat base-file-name ".c"))
	    (cc-case (concat base-file-name ".cc"))
	    (cpp-case (concat base-file-name ".cpp")))
	(cond
	 ((file-exists-p c-case)   (setq corresponding-file c-case))
	 ((file-exists-p cc-case)  (setq corresponding-file cc-case))
	 ((file-exists-p cpp-case) (setq corresponding-file cpp-case))))))
    (if corresponding-file
	(find-file corresponding-file)
      (error "Unable to find a file corresponding to %s" corresponding-file))))

;;; Paths:

;; Set directory variables depending on the current system
(defconst mug/home-dir (mug/win-or-linux (getenv "USERPROFILE") "~/"))
(defconst mug/cache-dir
  (mug/win-or-linux (getenv "LOCALAPPDATA")
                    "~/.cache"))
(defconst mug/emacs-dir (concat mug/home-dir "/.config/mugdot/emacs"))
(defconst mug/dev-dir (mug/win-or-linux "d:/"
					(concat mug/home-dir "/Projects")))

;; Set the default directories
(setq default-directory    mug/dev-dir
      custom-file          (concat mug/emacs-dir "/custom.el")
      user-emacs-directory mug/emacs-dir)

;;; Caching directories:

(defconst mug/emacs-cache-dir (concat mug/cache-dir "/emacs"))
(defconst mug/emacs-backup-dir (concat mug/emacs-cache-dir "/backups"))

;; Create the caching directories if non-existent
(if (not (file-exists-p mug/emacs-cache-dir))
    (make-directory mug/emacs-cache-dir))
(if (not (file-exists-p mug/emacs-backup-dir))
    (make-directory mug/emacs-backup-dir))

;; Set the backup directory
(setq backup-directory-alist `(("." . ,mug/emacs-backup-dir)))

;;; Straight bootstrapping:

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;;; Packages with no additional configuration:

(straight-use-package 'rg)
(straight-use-package 'magit)

;;; Emacs global configs:

(use-package emacs
  :init
  ;; Start emacs with a dired view to the default startup directory
  (find-file ".")
  :hook
  (before-save      . delete-trailing-whitespace)
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  ;; Allow foreing themes without asking
  (custom-safe-themes t)

  ;; Stop Emacs from losing undo information by setting very high
  ;; limits for undo buffers
  (undo-limit        20000000)
  (undo-strong-limit 40000000)

  ;; Put a limit on the *Messages* buffer
  (message-log-max 16384)

  ;; Prefer opening the latest version of the file
  (load-prefer-newer t)

  ;; Emacs backups and auto-saves
  (make-backup-files    t)
  (backup-by-copying    t)
  (kept-new-versions    10)
  (kept-old-versions    3)
  (delete-old-versions  t)   ; Clean the backup directory
  (version-control      t)   ; Use numbering of the backup files
  (vc-make-backup-files t)   ; Store even files controlled by a version-control system
  (auto-save-default    nil) ; We'll use the super-save package

  (completion-ignored-extensions
   '(".a"
     ".aux"
     ".bbl"
     ".bin"
     ".blg"
     ".elc"
     ".o"
     ".so"
     ".toc"
     "~"))

  (show-paren-delay 0)
  (indent-tabs-mode nil)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)

  ;; Horrible default UI stuff.
  (column-number-mode         -1)
  (display-line-numbers-mode  -1)
  (mode-line-percent-position nil)
  (menu-bar-mode              nil)
  (tool-bar-mode              nil)
  (scroll-bar-mode            nil)
  (tooltip-mode               -1)
  (ring-bell-function         'ignore)
  (visible-bell               nil)
  (inhibit-startup-message    t)
  (initial-scratch-message    "")

  ;; Scrolling settings
  (scroll-step           3)
  (scroll-conservatively 101)
  (auto-window-vscroll   nil)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)

  ;; Disable the eldoc mode
  (remove-hook 'after-change-major-mode-hook 'global-eldoc-mode-enable-in-buffers)
  (global-eldoc-mode -1)

  ;; Font
  (set-face-attribute 'default nil
                      :font (mug/win-or-linux "Terminus (TTF) for Windows"
                                              "Terminus (TTF)")
                      :height 120
		      :weight 'bold)


  ;; Window resizing.
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Escape from anything
  (keymap-global-set "<escape>" 'keyboard-escape-quit)
  (define-key key-translation-map (kbd "C-k") (kbd "<escape>"))

  ;; Annoying keybindings.
  (keymap-global-unset   "C-h h")
  (keymap-global-unset   "C-h k")
  (keymap-global-unset   "C-h j")
  (keymap-global-unset   "C-h l")
  (keymap-global-unset "C-x C-z")
  (keymap-global-unset     "C-z")
  (keymap-global-unset   "C-x s")
  (keymap-global-unset   "C-x f")
  (keymap-global-unset "C-x C-b")

  ;; Fucked-up solution to my emacs pinky:
  ;; This is needed for me because my keyboard uses the Caps Lock as an Fn key
  ;; when pressed together with some other key, in order to prevent such a fucking
  ;; bullshit, I translated every possible shit that could happen into the right
  ;; thing that it should be binded to in the first place.
  (define-key key-translation-map (kbd "C-<up>")           (kbd "C-w"))
  (define-key key-translation-map (kbd "C-<left>")         (kbd "C-a"))
  (define-key key-translation-map (kbd "C-<down>")         (kbd "C-s"))
  (define-key key-translation-map (kbd "C-<right>")        (kbd "C-d"))
  (define-key key-translation-map (kbd "C-<print>")        (kbd "C-p"))
  (define-key key-translation-map (kbd "C-<prt-scr>")      (kbd "C-p"))
  (define-key key-translation-map (kbd "C-<print-screen>") (kbd "C-p"))
  (defalias 'yes-or-no-p 'y-or-n-p)
  )

(use-package super-save
  :ensure t
  :commands (super-save-mode)
  :config
  (super-save-mode +1)
  :custom
  (super-save-silent t))

(use-package crux
  :bind
  ("C-c o" . crux-open-with)
  ("C-c I" . crux-find-user-init-file)
  ("C-c K" . crux-kill-other-buffers)
  ("C-x p" . crux-switch-to-previous-buffer)
  :config
  (add-hook 'emacs-lisp-mode-hook
	    (lambda () (local-set-key (kbd "C-c e") 'eval-buffer))))

(use-package which-key
  :custom
  (which-key-idle-delay 1.0)
  (which-key-mode))

(use-package vertico
  :commands (vertico-mode)
  :init     (vertico-mode)
  :custom
  (vertico-cycle t))

(use-package orderless
  :custom
  (completion-styles             '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package base16-theme
  :init (load-theme 'base16-gruvbox-material-dark-hard))

;; Better delimiter coloring for emacs
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dired
  :straight nil
  :config
  (setq dired-listing-switches "-agho --group-directories-first")

  (put 'dired-find-alternate-file 'disabled nil)

  (use-package dired-single)
  (use-package dired-ranger)
  (use-package dired-collapse))

(use-package evil
  :ensure t
  :commands (evil-mode evil-yank evil-global-set-key)
  :init
  (setq-default evil-want-keybinding nil)
  (setq evil-want-integration                  t
	evil-want-C-u-scroll                 nil
	evil-want-C-i-jump                   nil
	evil-respect-visual-line-mode          t
	evil-undo-system              'undo-redo
	evil-shift-width                       4
	evil-insert-state-cursor            'box
	evil-normal-state-cursor            'box
	evil-esc-delay                         0)
  :config
  (evil-mode 1)

  (defun mug/evil-hook ()
    "My evil mode."
    (dolist (mode '(custom-mode
                    eshell-mode
                    git-rebase-mode
                    term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))

  (defun mug-evil/yank-to-end-of-line ()
    "Yank to end of line."
    (interactive)
    (evil-yank (point) (line-end-position)))

  (add-hook 'evil-mode-hook 'mug/evil-hook)

  ;; Keybindings:
  (define-key evil-normal-state-map (kbd "Y") 'mug-evil/yank-to-end-of-line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :commands (evil-collection-init evil-collection-define-key)
  :init     (evil-collection-init)
  :custom   (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))

  (evil-collection-define-key 'normal 'dired-mode-map
			      "h" (lambda () (interactive) (find-alternate-file ".."))
			      "H" 'dired-omit-mode
			      "l" 'dired-single-buffer
			      "y" 'dired-ranger-copy
			      "X" 'dired-ranger-move
			      "p" 'dired-ranger-paste))

;; Nerd commenter is independent of Evil!
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;;; Latex configuration:

;; (use-package latex
;;   :mode ("\\.tex\\'" . LaTeX-mode)
;;   :straight auctex
;;   :hook ((LaTeX-mode . yas-minor-mode)
;;          (LaTeX-mode . TeX-source-correlate-mode)
;;          (LaTeX-mode . flyspell-mode)
;; 	 (LaTeX-mode . outline-minor-mode))
;;   :bind (:map LaTeX-mode-map
;; 	      ("<C-tab>" . outline-toggle-children)
;; 	      ("C-c C-c" . tex-compile)
;; 	      ("C-g C-q" . LaTeX-fill-paragraph)
;; 	      ("C-f C-r" . reftex-cleveref-cref))
;;   :config
;;   ;; Basic settings
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)
;;   (setq visual-fill-column-center-text t)

;;   (defun reftex-format-cref (label def-fmt)
;;     (format "\\cref{%s}" label))
;;   (setq reftex-format-ref-function 'reftex-format-cref)

;;   ;; Indentation settings
;;   (setq LaTeX-indent-level 0
;; 	LaTeX-item-indent -2)
;;         TeX-PDF-mode t
;;         TeX-source-correlate-mode t
;;         TeX-source-correlate-start-server t)

;;   ;; References setup
;;   (setq-default reftex-plug-into-AUCTeX t)

;;   (use-package cdlatex
;;     :after yasnippet
;;     :hook ((LaTeX-mode  . turn-on-cdlatex)
;; 	   (cdlatex-tab . yas-expand)
;;            (cdlatex-tab . mug/cdlatex-in-yas-field))
;;     :bind (:map cdlatex-mode-map
;; 		("<tab>" . cdlatex-tab))
;;     :config
;;     (keymap-local-unset "^")
;;     (setq cdlatex-use-dollar-to-ensure-math 0 ;; disable the use of dollar signs
;; 	  cdlatex-paired-parens "{(["
;; 	  cdlatex-simplify-sub-super-scripts t)

;;     ;; YaSnippet integration
;;     (defun mug/cdlatex-in-yas-field ()
;;       ;; Check if we're at the end of the Yas field
;;       (when-let* ((_ (overlayp yas--active-field-overlay))
;;                   (end (overlay-end yas--active-field-overlay)))
;;         (if (>= (point) end)
;;             ;; Call yas-next-field if cdlatex can't expand here
;;             (let ((s (thing-at-point 'sexp)))
;;               (unless (and s (assoc (substring-no-properties s)
;;                                     cdlatex-command-alist-comb))
;;                 (yas-next-field-or-maybe-expand)
;;                 t))
;;           ;; otherwise expand and jump to the correct location
;;           (let (cdlatex-tab-hook minp)
;;             (setq minp
;;                   (min (save-excursion (cdlatex-tab)
;;                                        (point))
;;                        (overlay-end yas--active-field-overlay)))
;;             (goto-char minp) t))))

;;     (defun yas-next-field-or-cdlatex nil
;;       "Jump to the next Yas field correctly with cdlatex active."
;;       (interactive)
;;       (if
;;           (or (bound-and-true-p cdlatex-mode)
;;               (bound-and-true-p org-cdlatex-mode))
;;           (cdlatex-tab)
;;         (yas-next-field-or-maybe-expand))))

;;     ;; Auto-expanding snippets
;;     (use-package aas
;;       :hook (LaTeX-mode . aas-activate-for-major-mode)
;;       :config
;;       (aas-set-snippets 'text-mode
;; 	".a" "ã"
;; 	".A" "Ã"
;; 	"/a" "â"
;; 	"/A" "Â"
;; 	";a" "á"
;; 	";A" "Á"
;; 	",a" "à"
;; 	",A" "À"
;; 	".o" "õ"
;; 	".O" "Õ"
;; 	"/o" "ô"
;; 	"/O" "Ô"
;; 	";o" "ó"
;; 	";O" "Ó"
;; 	";e" "é"
;; 	";E" "É"
;; 	"/e" "ê"
;; 	"/E" "Ê"
;; 	";c" "ç"
;; 	";i" "í"
;; 	";I" "Í"
;; 	";u" "ú"
;; 	";U" "Ú")
;;       (aas-set-snippets 'latex-mode
;; 	;; Math environments
;; 	"mk" (lambda () (interactive)
;;                "Inline math"
;;                (yas-expand-snippet "\\\\($0\\\\)"))
;; 	"!m" (lambda () (interactive)
;;                "Display math"
;;                (yas-expand-snippet "\\[\n$0\n\\]"))
;; 	"!ali" (lambda () (interactive)
;; 		 "align environment"
;; 		 (yas-expand-snippet "\\begin{align*}\n$0\n\\end{align*}"))
;; 	"!g" (lambda () (interactive)
;;                "gather environment"
;;                (yas-expand-snippet "\\begin{gather*}\n$0\n\\end{gather*}"))
;; 	"!eq" (lambda () (interactive)
;; 		"equation environment"
;; 		(yas-expand-snippet "\\begin{equation}\\label{eq:$1}\n$0\n\\end{equation}"))

;; 	"!beg" (lambda () (interactive)
;;                  "begin environment"
;;                  (yas-expand-snippet
;;                   "\\begin{$1}\n$0\n\\end{$1}"))
;; 	"!enum" (lambda () (interactive)
;;                   "enumerate environment"
;;                   (yas-expand-snippet
;;                    "\\begin{enumerate}[(a)]\\setlength\\itemsep{0em}\n\\item$0\n\\end{enumerate}"))
;; 	"!item" (lambda () (interactive)
;;                   "itemize environment"
;;                   (yas-expand-snippet
;;                    "\\begin{itemize}\\setlength\\itemsep{0em}\n\\item$0\n\\end{itemize}"))
;; 	:cond #'texmathp
;; 	"->" "\\to"
;; 	"-->" "\\longrightarrow"
;; 	"!>" "\\mapsto"
;; 	"ox" "\\otimes"
;; 	"opp" "\\oplus"
;; 	"OO" "\\infty"
;; 	"**" "\\times"
;; 	"cc" "\\subseteq"
;; 	";sm" "\\setminus"
;; 	"inn" "\\in"
;; 	"inv" "^{-1}"
;; 	"!=" "\\neq"
;; 	":=" "\\coloneq"
;; 	"==" "\\iso"
;; 	"//" (lambda () (interactive)
;;                "Fraction"
;;                (yas-expand-snippet "\\frac{$1}{$2}$0"))
;; 	"tt" (lambda () (interactive)
;;                "Text in math environment"
;;                (yas-expand-snippet "\\text{$1}$0"))
;; 	";set" (lambda () (interactive)
;; 		 "Collection --- set"
;; 		 (yas-expand-snippet "\\\\{$1\\\\}$0"))
;; 	"diag" (lambda () (interactive)
;; 		 "diagram environment"
;; 		 (yas-expand-snippet "\\begin{tikzcd}\n  $0\n\\end{tikzcd}"))
;; 	"bmat" (lambda () (interactive)
;; 		 "Matrix"
;; 		 (yas-expand-snippet "\\begin{bmatrix}\n  $0\n\\end{bmatrix}"))))

(provide 'init)
;;; Local Variables:
;;; init.el ends here
