;;; Configurations
;;; Code:

;; Font size:
(set-face-attribute 'default nil :height 120)
(add-to-list 'default-frame-alist '(font . "Hasklug Nerd Font"))
(set-face-attribute 'default t :font "Hasklug Nerd Font")

;; Line numbers:
(column-number-mode) (global-display-line-numbers-mode t)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook

                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
	      	term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Filesystem backups
(setq
 make-backup-files nil ;; disable backup
 ;; auto-save-default nil
 create-lockfiles nil)

;; Highlight at current line
(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")

;; Keybindings:
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; ESC quit prompts
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)   ;; Moves through buffers with C-M-j

;; Deal with whitespaces
(add-hook 'before-save-hook #'delete-trailing-whitespace)
(setq require-final-newline t)

;; EMACS LOOK
(setq inhibit-startup-message t) ;; Disable ugly startup message

;; Inhibit ugly top of the window stuff
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Transparency
(defvar efs/frame-transparency '(90 . 90))

;; Package configuration
(require 'package)

(setq package-archives '(("org" . "http://orgmode.org/elpa/")
			  ("melpa" . "http://melpa.org/packages/")
			  ("melpa-stable" . "http://stable.melpa.org/packages/")
			  ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Packages

;; ivy: Command completion
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         ("C-M-l" . counsel-imenu)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; Git stuff with Magit
(use-package magit)

;; Install icons for the themes
;; On first time run: M-x all-the-icons-install-fonts
(use-package all-the-icons)

;; A better mode line from Doom
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

;; Better delimiter coloring for emacs
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Tells you the possible keys for certain commands
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(defun luiz/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
                  git-rebase-mode
                  erc-mode
                  circe-server-mode
                  circe-chat-mode
                  circe-query-mode
                  sauron-mode
                  term-mode))
  (add-to-list 'evil-emacs-state-modes mode)))

;; Sorry Emacs nerd, but Vim rules
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree) ;; Requires undo-tree-mode
  :config
  (add-hook 'evil-mode-hook 'luiz/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Nerd commenter for Emacs: toggle comment with M-/
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Allows the use of u for undos in evil mode
(use-package undo-tree)
(global-undo-tree-mode)

;; ;; Tab support for emacs
;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   :custom
;;   (centaur-tabs-gray-out-icons 'buffer)
;;   (centaur-tabs-style "rounded")
;;   (centaur-tabs-height 36)
;;   (centaur-tabs-set-icons t)
;;   (centaur-tabs-set-modified-marker t)
;;   (centaur-tabs-modified-marker "●")
;;   (centaur-tabs-buffer-groups-function #'centaur-tabs-projectile-buffer-groups)
;;   :bind
;;   (("C-<" . #'centaur-tabs-backward)
;;    ("C->" . #'centaur-tabs-forward)))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :demand t
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map) ;; Every projectile command starts with C-c p
  :init
  (when (file-directory-p "~/luizm/math")
    (setq projectile-project-search-path '("~/luizm/math")))
  (setq projectile-switch-project-action #'projectile-dired))


;; Language stuff: language server protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred)
	(haskell-mode . lsp-deferred)
	(lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-enable-file-watchers nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :commands lsp-ui-mode)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))


(use-package yasnippet
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;; Built-in Python utilities
(use-package python
  :ensure t
  :config
  ;; Remove guess indent python message
  (setq python-indent-guess-indent-offset-verbose nil)
  ;; Use IPython when available or fall back to regular Python
  (cond
   ((executable-find "ipython")
    (progn
      (setq python-shell-buffer-name "IPython")
      (setq python-shell-interpreter "ipython")
      (setq python-shell-interpreter-args "-i --simple-prompt")))
   ((executable-find "python3")
    (setq python-shell-interpreter "python3"))
   ((executable-find "python2")
    (setq python-shell-interpreter "python2"))
   (t
    (setq python-shell-interpreter "python"))))


(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

;; Haskell lsp
(use-package haskell-mode)

(setq haskell-process-type 'ghci)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'hindent-mode)


;; (custom-set-variables '(haskell-process-type 'stack-ghci))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp)
  :custom
  (lsp-haskell-process-path-hie "ghcid")
  (lsp-haskell-process-args-hie '())
  (lsp-log-io t))
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package ormolu)

(use-package haskell-snippets
  :ensure t
  :after (haskell-mode yasnippet)
  :defer)

;; Maps the location of the custom stuff to a better place
(setq custom-file "~/.emacs.d/custom.el")

(provide 'init)
;;; init.el ends here
