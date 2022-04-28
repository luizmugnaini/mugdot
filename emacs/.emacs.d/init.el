;; These are my configurations for GNU Emacs

;;; Code:

;; Package configuration -------------------------------------------------------

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

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))


;; Font setup ------------------------------------------------------------------

(defvar efs/default-font-size 140)
(defvar efs/default-variable-font-size 140)
(set-face-attribute 'default nil :font "Fira Code" :height efs/default-font-size)
;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height efs/default-variable-font-size)

;; Font ligatures!
(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
  :config (global-fira-code-mode)
  ;; Enables fira-code-mode automatically for programming major modes
  :hook prog-mode)


;; Lines: numbers and length ---------------------------------------------------

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

;; Highlight at current line
(require 'hl-line)
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(set-face-attribute 'hl-line nil :background "gray21")

;; Wrapping lines with > 80 chars
(setq-default fill-column 80)
(add-hook 'text-mode-hook #'auto-fill-mode)
(add-hook 'prog-mode-hook #'auto-fill-mode)


;; Filesystem backups ----------------------------------------------------------

(setq
 make-backup-files nil ; disable backup
 ;; auto-save-default nil
 create-lockfiles nil)

;; Use no-littering to automatically set common paths to the new
;; user-emacs-directory
(use-package no-littering)


;; .emacs.d dir ----------------------------------------------------------------

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/")
      url-history-file (expand-file-name "url/history" user-emacs-directory))


;; Emacs behaviour: ------------------------------------------------------------

;; Deal with whitespaces
(add-hook 'before-save-hook #'delete-trailing-whitespace)

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
(xclip-mode 1)

;; Transparency settings
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Keybindings:
; ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
; Moves through buffers with C-M-j
(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)


;; ivy: Command completion ------------------------------------------------------

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


;; Terminal -----------------------------------------------------------------------

(use-package vterm
    :ensure t)


;; Good utilities: ------------------------------------------------------------

(use-package crux)
(global-set-key (kbd "C-c o") #'crux-open-with)
(global-set-key (kbd "C-c I") #'crux-find-user-init-file)
(global-set-key (kbd "C-c k") #'crux-kill-other-buffers)
(global-set-key (kbd "C-c e") 'eval-buffer)

;; Tells you the possible keys for certain commands
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))


;; Git stuff with Magit -------------------------------------------------------

(use-package magit)


;; Icons, modeline and themes -------------------------------------------------

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


;; Evil ------------------------------------------------------------------------

(defun mug/evil-hook ()
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
  (add-hook 'evil-mode-hook 'mug/evil-hook)
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


;; Project management ----------------------------------------------------------

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :demand t
  :bind ("C-M-p" . projectile-find-file)
  :bind-keymap
  ;; Every projectile command starts with C-c p
  ("C-c p" . projectile-command-map)
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


;; Dired -----------------------------------------------------------------------

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


;; LSP setup -------------------------------------------------------------------

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (python-mode . lsp-deferred)
	(haskell-mode . lsp-deferred)
	(rust-mode . lsp-deferred)
	(julia-mode . lsp-deferred)
	(LaTeX-mode . lsp-deferred)
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

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))


;; Completion and snippets -----------------------------------------------------

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

;; YaSnippets configuration
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.mugdot/emacs/.emacs.d/snippets/")))

;; Disable addition of newline to final of snippet:
;; For future me --- if you run into trouble again with this shit, set the two
;; following lines and they will solve the problem (I only hope so):
;; (setq mode-require-final-newline nil)
;; (setq require-final-newline nil)
(defun final-nl ()
  (interactive) (set (make-local-variable 'require-final-newline) nil))
(add-hook 'snippet-mode-hook 'final-nl)

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
    ;; Common portuguese accents
    ";-a" "ã"
    ";-A" "Ã"
    ";^a" "â"
    ";^A" "Â"
    ";'a" "á"
    ";'A" "Á"
    ";`a" "à"
    ";`A" "À"

    ";-o" "õ"
    ";-O" "Õ"
    ";^o" "ô"
    ";^O" "Ô"
    ";'o" "ó"
    ";'O" "Ó"

    ";'e" "é"
    ";'E" "É"
    ";^e" "ê"
    ";^E" "Ê"

    ";c" "ç"
    ";'i" "í"
    ";'I" "Í"
    ";'u" "ú"
    ";'U" "Ú"))


;; LaTeX setup -----------------------------------------------------------------

;;; AuCTeX configuration

;; Currently the support for autocompilation is a mess:
;; open a terminal and run `latexmk -pvc` on the project of interest
(use-package latex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode)
	 ;; Make AUCTeX aware of multifile doc structure
	 (LaTeX-mode . TeX-source-correlate-mode))
  :bind (:map LaTeX-mode-map
         ("C-M-w" . latex-math-from-calc))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  ;; Format math as a Latex string with Calc
  ;; Use C-S-e
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

;; Somewhat of a latexmk integration with AucTeX
(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


;;; Visual editing

;; PDF previewing with zathura and sinctex enabled
(add-to-list 'TeX-expand-list
	     '("%sn" (lambda () server-name)))
(add-to-list 'TeX-view-program-list
   '("Zathura"
   ("zathura %o"
    (mode-io-correlate " --synctex-forward %n:0:"%b" -x \"emacsclient --socket-name=%sn +%{line} %{input}\""))
   "zathura"))
(setcar (cdr (assoc 'output-pdf TeX-view-program-selection)) "Zathura")

;; Visualization with buffer preview --- use C-c C-p
(add-hook 'LaTeX-mode-hook
          (defun preview-larger-previews ()
            (setq preview-scale-function
                  (lambda () (* 1.25
                           (funcall (preview-scale-from-face)))))))


;;; Latex lsp

(use-package lsp-latex
  :config
  (setq lsp-latex-texlab-executable "~/.cargo/bin/texlab")
  (setq lsp-latex-forward-search-executable "zathura")
  (setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p")) )
(with-eval-after-load "LaTeX-mode"
 (add-hook 'TeX-mode-hook 'lsp)
 (add-hook 'LaTeX-mode-hook 'lsp))


;;; Latex text input configuration:

;; cdlatex stuff
(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
	 ;; YaSnippet integration
	 (cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab))
  :config
  ;; Environments
  (setq cdlatex-env-alist
     '(("proposition" "\\begin{proposition}\n\\label{prop:$1}\n$0\n\\end{proposition}\n" nil)
       ("theorem" "\\begin{theorem}\n\\label{thm:}\n?\n\\end{theorem}\n" nil)
       ("corollary" "\\begin{corollary}\n\\label{cor:}\n?\n\\end{corollary}\n" nil)
       ("definition" "\\begin{definition}\n\\label{def:}\n?\n\\end{definition}\n" nil)
       ("proof" "\\begin{proof}\n?\n\\end{proof}\n" nil)
       ("diagram" "\\begin{tikzcd}\n?\n\\end{tickzcd}")))

  ;; Commands
  (setq cdlatex-command-alist
    '(("prop" "Insert proposition env" "" cdlatex-environment ("proposition") t nil)
      ("thm" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)
      ("cor" "Insert corollary env" "" cdlatex-environment ("corollary"))
      ("def" "Insert definition env" "" cdlatex-environment ("definition"))
      ("proof" "Insert proof env" "" cdlatex-environment ("proof"))
      ("diag" "Insert diagram env " "" cdlatex-environment ("diagram")))))

;; YaSnippet integration for cdlatex
;; I'm not using this, but I'll let this sit here for now, I may change my mind
;; and try it out. To make this actually work, just add the following code to
;; the :config section of cdlatex:
;;
;; (use-package yasnippet
;; :bind (:map yas-keymap
;; 	("<tab>" . yas-next-field-or-cdlatex)
;; 	("TAB" . yas-next-field-or-cdlatex))
;; :config
;; (defun cdlatex-in-yas-field ()
;;     ;; Check if we're at the end of the Yas field
;;     (when-let* ((_ (overlayp yas--active-field-overlay))
;; 		(end (overlay-end yas--active-field-overlay)))
;;     (if (>= (point) end)
;; 	;; Call yas-next-field if cdlatex can't expand here
;; 	(let ((s (thing-at-point 'sexp)))
;; 	    (unless (and s (assoc (substring-no-properties s)
;; 				cdlatex-command-alist-comb))
;; 	    (yas-next-field-or-maybe-expand)
;; 	    t))
;; 	;; otherwise expand and jump to the correct location
;; 	(let (cdlatex-tab-hook minp)
;; 	(setq minp
;; 		(min (save-excursion (cdlatex-tab)
;; 				    (point))
;; 		    (overlay-end yas--active-field-overlay)))
;; 	(goto-char minp) t))))
;;
;; (defun yas-next-field-or-cdlatex nil
;;     (interactive)
;;     "Jump to the next Yas field correctly with cdlatex active."
;;     (if
;; 	(or (bound-and-true-p cdlatex-mode)
;; 	    (bound-and-true-p org-cdlatex-mode))
;; 	(cdlatex-tab)
;;     (yas-next-field-or-maybe-expand))))

;; Auto-expanding snippets
(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
		    ;; Math environments
                    "mk" (lambda () (interactive)
			   "Inline math"
                          (yas-expand-snippet "\\\\($1\\\\)$0"))
		    "dm" (lambda () (interactive)
			   "Display math"
			   (yas-expand-snippet "\\[\n$1\n\\]$0"))
                    :cond #'texmathp
		    "NN" "\\N"
		    "ZZ" "\\Z"
		    "QQ" "\\Q"
		    "RR" "\\R"
		    "CC" "\\C"
		    "ox" "\\otimes"
		    "o+" "\\oplus"
		    "iso" "\\iso"
		    "cc" "\\subseteq"
		    "sm" "\\setminus"
		    "tt" (lambda () (interactive)
			    "Text in math environment"
			    (yas-expand-snippet "\\text{$1}$0"))
		    "set" (lambda () (interactive)
			    "Set"
			    (yas-expand-snippet "\\\\{$1\\\\}$0"))
                    "Sum" (lambda () (interactive)
			    "Summation with limitsk"
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Int" (lambda () (interactive)
			    "Definite integral"
                            (yas-expand-snippet "\\int_{$1}^{$2} $0"))
		    "bmat" (lambda () (interactive)
			     "Matrix"
			     (yas-expand-snippet "\\begin{bmatrix}\n$1\n\\end{bmatrix}$0")
		    "cases" (lambda () (interactive)
			     "Cases environment"
			     (yas-expand-snippet "\\begin{cases}\n$1\n\\end{cases}$0")
		    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive)
			   "Wrap sqrt on the left object"
			   (laas-wrap-previous-object "sqrt"))
		    "inv" (lambda () (interactive)
			    "Inverse"
			    (yas-expand-snippet "^{-1}"))))


;; Rust setup ------------------------------------------------------------------

(use-package rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))
(setq rust-format-on-save t)
(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;; Python setup ----------------------------------------------------------------

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

;; Julia -----------------------------------------------------------------------

(use-package julia-mode)
(add-to-list 'load-path "/usr/bin/julia")
(use-package julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7"))
(use-package flycheck-julia)
(flycheck-julia-setup)

;; Haskell ---------------------------------------------------------------------

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
  (lsp-haskell-process-path-hie "cabal repl")
  (lsp-haskell-process-args-hie '())
  (lsp-log-io t))
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(use-package haskell-snippets
  :ensure t
  :after (haskell-mode yasnippet)
  :defer)

;; Org mode --------------------------------------------------------------------

(defun mug/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 0) ;; Fira is horrible with this thing on
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . mug/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Side columns filling to make org mode more presentable
(defun mug/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
    visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mug/org-mode-visual-fill))


;; Using latex snippets inside org mode
;; (defun my-org-latex-yas ()
;;   "Activate org and LaTeX yas expansion in org-mode buffers."
;;   (yas-minor-mode)
;;   (yas-activate-extra-mode 'latex-mode))
;; (add-hook 'org-mode-hook #'my-org-latex-yas)

;; important stuff ends here ---------------------------------------------------

;; Maps the location of the custom stuff to a better place
(setq custom-file "~/.emacs.d/custom.el")

(provide 'init)
;;; init.el ends here
