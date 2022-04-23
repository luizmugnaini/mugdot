;;; package -- Summary
;;; Commentary:
;; These are my configurations for GNU Emacs

;;; Code:

;; Font:
(defvar efs/default-font-size 140)
(defvar efs/default-variable-font-size 140)
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-variable-font-size)

;; Line numbers:
(column-number-mode) (global-display-line-numbers-mode t)

;; Wrapping lines with > 80 chars
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
	      	term-mode-hook
		shell-mode-hook
		eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Filesystem backups:
(setq
 make-backup-files nil ; disable backup
 ;; auto-save-default nil
 create-lockfiles nil)

;; .emacs.d dir:
;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))

;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Highlight at current line
(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")

;; Keybindings:
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; ESC quit prompts
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)   ; Moves through buffers with C-M-j

;; Deal with whitespaces
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Emacs look:
(setq inhibit-startup-message t) ;; Disable ugly startup message
(menu-bar-mode -1)

;; Inhibit ugly top of the window stuff
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Emacs behaviour:
(setq scroll-step 1)
(setq scroll-conservatively 10000)
(setq auto-window-vscroll nil)

;; Package configuration:
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

(xclip-mode 1)

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


(use-package vterm
    :ensure t)

;; Good utilities:
(use-package crux)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key (kbd "C-c I") #'crux-find-user-init-file)
(global-set-key (kbd "C-c k") #'crux-kill-other-buffers)
(global-set-key (kbd "C-c e") 'eval-buffer)

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
  :init (load-theme 'doom-palenight t))

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
  "My evil mode."
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
  (setq evil-undo-system 'undo-fu) ;; Requires undo-tree-mode
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
  :init
  (setq evil-collection-company-use-tng nil)
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (setq evil-collection-mode-list
        (remove 'lispy evil-collection-mode-list))
  (evil-collection-init))

;; Nerd commenter for Emacs: toggle comment with M-/
(use-package evil-nerd-commenter
  :bind ("M-/" . evilnc-comment-or-uncomment-lines))

;; Allows the use of u for undos in evil mode
(use-package undo-fu)

;; Projectile: working with projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :demand t
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ("C-c p" . projectile-command-map) ;; Every projectile command starts with C-c p
  :init
  (when (file-directory-p "~/luiz/Projects")
    (setq projectile-project-search-path '("~/luiz/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;; buffers by projects
(use-package perspective
  :ensure t  ; use `:straight t` if using straight.el!
  :bind
  (("C-x k" . persp-kill-buffer*))
  (("C-x b" . persp-counsel-switch-buffer))
  :custom
  (persp-mode-prefix-key (kbd "C-x x"))
  (persp-suppress-no-prefix-key-warning t)
  :init
  (persp-mode))

;; Saving Sessions:
(desktop-save-mode 1)
(defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook 'my-desktop-save)

;; File management
(use-package all-the-icons-dired)
(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-listing-switches "-agho --group-directories-first")

  (use-package dired-single
    :defer t)

  (use-package dired-ranger
    :defer t)

  (use-package dired-collapse
    :defer t)

  (evil-collection-define-key 'normal 'dired-mode-map
    "h" (lambda () (interactive) (find-alternate-file ".."))
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste))
(put 'dired-find-alternate-file 'disabled nil)

;; Image viewing within emacs
(setq image-dired-external-viewer "feh")
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
  (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external))

;; Font ligatures!
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :config (global-fira-code-mode)
  :hook prog-mode) ; Enables fira-code-mode automatically for programming major modes

;; Language stuff: language server protocol
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred)
	(haskell-mode . lsp-deferred)
	(rust-mode . lsp-deferred)
	(julia-mode . lsp-deferred)
	;; (LaTeX-mode . lsp-deferred)
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

;; Completion stuff
(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

;; Icons for company completion
(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.mugdot/emacs/.emacs.d/snippets/"
			   "~/.mugdot/emacs/.emacs.d/snippets/plain-tex-mode"))
  (yas-global-mode))

;; Disable addition of newline to final of snippet
(defun final-nl ()
  (interactive) (set (make-local-variable 'require-final-newline) nil))
(add-hook 'snippet-mode-hook 'final-nl)
;; (setq mode-require-final-newline nil)
;; (setq require-final-newline nil)

;; LaTeX:
;; Currently the support for autocompilation is a mess:
;; open a terminal and run `latexmk -pvc` on the project of interest
(use-package tex
  :ensure auctex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq reftex-plug-into-AUCTeX t)

;; Somewhat of a latexmk integration with auctex --- doesn't solve my problem
(use-package auctex-latexmk)
(auctex-latexmk-setup)
(setq auctex-latexmk-inherit-TeX-PDF-mode t)

;; Make AUCTeX aware of multifile doc structure
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;; PDF previewing with zathura and sinctex enabled
(setq TeX-PDF-mode t
   TeX-source-correlate-mode t
   TeX-source-correlate-start-server t)

(add-to-list 'TeX-expand-list
	     '("%sn" (lambda () server-name)))

(add-to-list 'TeX-view-program-list
   '("Zathura"
   ("zathura %o"
    (mode-io-correlate " --synctex-forward %n:0:"%b" -x \"emacsclient --socket-name=%sn +%{line} %{input}\""))
   "zathura"))

(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")

;; Use Cleveref
(eval-after-load "latex"
  '(TeX-add-style-hook "cleveref"
    (lambda ()
      (if (boundp 'reftex-ref-style-alist)
      (add-to-list
       'reftex-ref-style-alist
       '("Cleveref" "cleveref"
         (("\\cref" ?c) ("\\Cref" ?C) ("\\cpageref" ?d) ("\\Cpageref" ?D)))))
      (reftex-ref-style-activate "Cleveref")
      (TeX-add-symbols
       '("cref" TeX-arg-ref)
       '("Cref" TeX-arg-ref)
       '("cpageref" TeX-arg-ref)
       '("Cpageref" TeX-arg-ref)))))

;; Latex lsp
(use-package lsp-latex)
(getenv "PATH")
;; (setenv "PATH" (concat (getenv "PATH") ":/home/luiz/.cargo/bin/"))
(setq lsp-latex-texlab-executable "~/.cargo/bin/texlab")
(with-eval-after-load "LaTeX-mode"
 (add-hook 'TeX-mode-hook 'lsp)
 (add-hook 'LaTeX-mode-hook 'lsp))
(setq lsp-latex-forward-search-executable "zathura")
(setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))

;; Rust
(use-package rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;; Python support:
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

;; Julia
(use-package julia-mode)
(add-to-list 'load-path "/usr/bin/julia")
(use-package julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7"))

;; Haskell lsp
(use-package haskell-mode)

(setq haskell-process-type 'ghci)
(setq haskell-process-log t)

(add-hook 'haskell-mode-hook 'haskell-indent-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'hindent-mode)

(custom-set-variables '(haskell-process-type 'stack-ghci))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp)
  :custom
  (lsp-haskell-process-path-hie "ghcid")
  (lsp-haskell-process-args-hie '())
  (lsp-log-io t))
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package haskell-snippets
  :ensure t
  :after (haskell-mode yasnippet)
  :defer)

;; Maps the location of the custom stuff to a better place
(setq custom-file "~/.emacs.d/custom.el")

(provide 'init)
;;; init.el ends here
