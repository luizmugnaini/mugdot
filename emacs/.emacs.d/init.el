;;; emacs --- My emacs configurations

;;; Commentary:
;; Luiz Mugnaini -> luizmugnaini@gmail.com

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

(defvar mug/default-font-size 140)
(defvar mug/default-variable-font-size 140)
;; (defvar symbola-font (font-spec :name "Symbola" :size 15))
(set-face-attribute 'default nil :font "Fira Code" :height mug/default-font-size)
(set-face-attribute 'fixed-pitch nil :font "Fira Code" :height mug/default-variable-font-size)

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

;; Transparency settings
(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Keybindings:
; ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key key-translation-map (kbd "C-k") (kbd "<escape>"))

; Moves through buffers with C-M-j
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


;; ivy: Command completion ------------------------------------------------------

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
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

(defun vterm-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when vterm--process
    (let* ((pid (process-id vterm--process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))


;; Good utilities: ------------------------------------------------------------

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


;; Git stuff with Magit -------------------------------------------------------

(use-package magit
  :commands magit-status)


;; Icons, modeline and themes -------------------------------------------------

;; Install icons for the themes
;; On first time run: M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

;; A better mode ooline from Doom
(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-challenger-deep t)) ;; last: palenight

;; Better delimiter coloring for emacs
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Sintax highlighting
(use-package tree-sitter)
(use-package tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


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
(setq-default evil-want-keybinding nil)
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree) ;; last: undo-fu
  (setq evil-shift-width 2)
  :config
  (add-hook 'evil-mode-hook 'mug/evil-hook)
  (evil-mode 1)

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
(use-package undo-tree)
(global-undo-tree-mode)

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
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired))


;; Dired -----------------------------------------------------------------------

;; File management
(use-package all-the-icons-dired
  :after dired)
(use-package dired
  :ensure nil
  :defer 1
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (setq dired-listing-switches "-agho --group-directories-first")

  ;; Image previewing
  (setq image-dired-external-viewer "feh")
  (define-key dired-mode-map (kbd "C-t C-d") 'image-dired)
  (define-key dired-mode-map (kbd "C-<return>") 'image-dired-dired-display-external)
  (put 'dired-find-alternate-file 'disabled nil)

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


;; LSP setup -------------------------------------------------------------------

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

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

(use-package company-tabnine
  :ensure t)


;; Spell checking --------------------------------------------------------------

;; (setq-default ispell-library-directory "/usr/share/myspell/dicts/")
;; (use-package flyspell
;; 	:hook ((LaTeX-mode . flyspell-mode))
;; 	:config
;; 	(setq ispell-program-name "hunspell")
;; 	(setq ispell-dictionary "en_GB")
;; 	(setq flyspell-default-dictionary "/usr/share/myspell/dicts/en_GB.aff")
;;   (setq flyspell-issue-message-flag nil)
;; 	(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t))


;; Completion and snippets -----------------------------------------------------

(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  (setq company-dabbrev-other-buffers t
        company-dabbrev-code-other-buffers t
        company-idle-delay 0
        company-show-numbers t)
  :hook ((text-mode . company-mode)
         (prog-mode . company-mode)))

;; Icons for company completion
(use-package company-box
  :hook (company-mode . company-box-mode))

;; YaSnippets configuration
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-hook . yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("~/.mugdot/emacs/.emacs.d/snippets/")))

;; Disable
(defun final-nl ()
  "Disable addition of newline to final of snippet.
For future me --- if you run into trouble again with this shit, set the two
following lines and they will solve the problem --- I can only hope so
`(setq mode-require-final-newline nil)`
`(setq require-final-newline nil)`"
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))
(add-hook 'snippet-mode-hook 'final-nl)

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
    ;; Common portuguese accents
    ".a" "ã"
    ".A" "Ã"
    "/a" "â"
    "/A" "Â"
    ";a" "á"
    ";A" "Á"
    ",a" "à"
    ",A" "À"

    ".o" "õ"
    ".O" "Õ"
    "/o" "ô"
    "/O" "Ô"
    ";o" "ó"
    ";O" "Ó"

    ";e" "é"
    ";E" "É"
    "/e" "ê"
    "/E" "Ê"

    ";c" "ç"
    ";i" "í"
    ";I" "Í"
    ";u" "ú"
    ";U" "Ú")
  (aas-set-snippets 'latex-mode
    ;; Math environments
    "mk" (lambda () (interactive)
           "Inline math"
           (yas-expand-snippet "\\\\($0\\\\)"))
    "dm" (lambda () (interactive)
           "Display math"
           (yas-expand-snippet "\\[\n  $0\n\\]"))
    "!ali" (lambda () (interactive)
             "align environment"
             (yas-expand-snippet "\\begin{align*}\n  $0\n\\end{align*}\n"))
    "!gather" (lambda () (interactive)
                "gather environment"
                (yas-expand-snippet "\\begin{gather*}\n  $0\n\\end{gather*}\n"))
    "!eq" (lambda () (interactive)
            "equation environment"
            (yas-expand-snippet "\\begin{equation}\\label{eq:$1}\n  $0\n\\end{equation}"))

    ;; Text environments
    "!prop" (lambda () (interactive)
               "proposition environment"
               (yas-expand-snippet "\\begin{proposition}\n\\label{prop:$1}\n$0\n\\end{proposition}"))
    "!thm" (lambda () (interactive)
               "theorem environment"
               (yas-expand-snippet "\\begin{theorem}\n\\label{thm:$1}\n$0\n\\end{theorem}"))
    "!proof" (lambda () (interactive)
               "proof environment"
               (yas-expand-snippet "\\begin{proof}\n$0\n\\end{proof}\n"))
    "!lem" (lambda () (interactive)
               "lemma environment"
               (yas-expand-snippet "\\begin{lemma}\n\\label{lem:$1}\n$0\n\\end{lemma}"))
    "!cor" (lambda () (interactive)
               "corollary environment"
               (yas-expand-snippet "\\begin{corollary}\n\\label{cor:$1}\n$0\n\\end{corollary}"))
    "!def" (lambda () (interactive)
                "definition environment"
                (yas-expand-snippet "\\begin{definition}\n\\label{def:$1}\n$0\n\\end{definition}"))
    "!exp" (lambda () (interactive)
                "example environment"
                (yas-expand-snippet "\\begin{example}\n\\label{exp:$1}\n$0\n\\end{example}"))
    "!rem" (lambda () (interactive)
                "remark environment"
                (yas-expand-snippet "\\begin{remark}\n\\label{rem:$1}\n$0\n\\end{remark}"))
    "!not" (lambda () (interactive)
                "notation environment"
                (yas-expand-snippet "\\begin{notation}\n\\label{not:$1}\n$0\n\\end{notation}"))
    "!enum" (lambda () (interactive)
                "enumerate environment"
                (yas-expand-snippet "\\begin{enumerate}\\setlength\\itemsep{0em}\n  \\item$0\n\\end{enumerate}"))
    "!item" (lambda () (interactive)
                "itemize environment"
                (yas-expand-snippet "\\begin{itemize}\\setlength\\itemsep{0em}\n  \\item$0\n\\end{itemize}"))

    :cond #'texmathp
      "NN" "\\N"
      "ZZ" "\\Z"
      "QQ" "\\Q"
      "RR" "\\R"
      "CC" "\\CC"

      ";0" "\\emptyset"

      "ell" "\\ell"
      "eps" "\\varepsilon"
      ";p" "\\phi"
      ";P" "\\Phi"
      ";S" "\\Psi"
      ";g" "\\gamma"
      ";w" "\\omega"
      ";W" "\\Omega"
      ";l" "\\lambda"
      ";m" "\\mu"
      ";d" "\\delta"
      ";D" "\\Delta"

      "iso" "\\iso"
      "->" "\\to"
      "-->" "\\longrightarrow"
      "!>" "\\mapsto"
      "!->" "\\longmapsto"

      "vv" "\\wedge"
      "ox" "\\otimes"
      "o+" "\\oplus"
      "OO" "\\infty"
      "dd" "\\diff"
      ",," "\\,"
      "**" "\\times"
      ";*" "^{*}"
      "cc" "\\subseteq"
      ";sm" "\\setminus"
      "inn" "\\in"
      "inv" "^{-1}"
      "..." "\\dots"

      "leq" "\\leq"
      "geq" "\\geq"
      "!=" "\\neq"
      ":=" "\\coloneq"
      "<=" "\\leq"
      ">=" "\\geq"

      "cal" (lambda () (interactive)
             "mathcal"
             (yas-expand-snippet "\\mathcal{$1}$0"))
      "//" (lambda () (interactive)
             "Text in math environment"
             (yas-expand-snippet "\\frac{$1}{$2}$0"))
      "tt" (lambda () (interactive)
             "Text in math environment"
             (yas-expand-snippet "\\text{$1}$0"))
      "?>" (lambda () (interactive)
             "xmapsto"
             (yas-expand-snippet "\\xmapsto{$1}$0"))
      ";set" (lambda () (interactive)
              "Collection --- set"
              (yas-expand-snippet "\\\\{$1\\\\}$0"))
      "norm" (lambda () (interactive)
               "Norm"
               (yas-expand-snippet "\\\\| $1 \\\\|$0"))
      "Sum" (lambda () (interactive)
              "Summation with limits"
              (yas-expand-snippet "\\sum_{$1}^{$2}$0"))
      "diag" (lambda () (interactive)
               "diagram environment"
               (yas-expand-snippet "\\begin{tikzcd}\n  $0\n\\end{tikzcd}"))
      "bmat" (lambda () (interactive)
               "Matrix"
               (yas-expand-snippet "\\begin{bmatrix}\n  $0\n\\end{bmatrix}"))
      "cases" (lambda () (interactive)
               "Cases environment"
               (yas-expand-snippet "\\begin{cases}\n $0\n\\end{cases}"))))


;; LaTeX setup -----------------------------------------------------------------

;;; AuCTeX configuration

;; Currently the support for autocompilation is a mess:
;; open a terminal and run `latexmk -pvc` on the project of interest
(use-package latex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :ensure auctex
  :hook ((LaTeX-mode . yas-minor-mode)
         ;; Make AUCTeX aware of multifile doc structure
         (LaTeX-mode . TeX-source-correlate-mode))
  :config
  ;; Basic settings
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq visual-fill-column-center-text t)

  (defun reftex-format-cref (label def-fmt) (format "\\cref{%s}" label))
  (setq reftex-format-ref-function 'reftex-format-cref)

  ;; Indentation settings
  (setq LaTeX-indent-level 0)
  (setq LaTeX-item-indent 0)

  ;; Compilation and PDF
  (setq TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  ;; References setup
  (setq-default reftex-plug-into-AUCTeX t)

  ;; Fold by section using C-tab
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (define-key LaTeX-mode-map (kbd "<C-tab>") 'outline-toggle-children))

;; Somewhat of a latexmk integration with AucTeX
(use-package auctex-latexmk
  :ensure t
  :config
  (auctex-latexmk-setup)
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))


;;; Visual editing

;; Visualization with buffer preview --- use C-c C-p
(add-hook 'LaTeX-mode-hook
          (defun preview-larger-previews ()
            (setq preview-scale-function
                  (lambda () (* 1.0 ;; Change this scale if need be
                           (funcall (preview-scale-from-face)))))))


;;; Latex lsp -- this really slows down the editing process and is not that
;;; worthy

;; (use-package lsp-latex
;;   :config
;;   (setq lsp-latex-texlab-executable "~/.cargo/bin/texlab")
;;   (setq lsp-latex-forward-search-executable "zathura")
;;   (setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))
;; 	(setq lsp-latex-diagnostics-delay 500))
;; (with-eval-after-load "LaTeX-mode"
;;  (add-hook 'TeX-mode-hook 'lsp)
;;  (add-hook 'LaTeX-mode-hook 'lsp))


;;; Latex text input configuration:


;; cdlatex stuff
(use-package cdlatex
  :ensure t
  :hook ((LaTeX-mode . turn-on-cdlatex)
	 (cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab))
  :config
  (setq cdlatex-takeover-subsuperscript nil)
  (setq cdlatex-takeover-parenthesis nil)
  (local-unset-key (kbd "^"))
  (setq cdlatex-paired-parens "{([")
  (setq cdlatex-sub-super-scripts-outside-math-mode nil)
  (setq cdlatex-simplify-sub-super-scripts t)

  ;; YaSnippet integration
  (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand))))


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
  (setq
    python-shell-interpreter "/home/luiz/.local/bin/ipython"
    python-shell-interpreter-args "--profile=mugipy -i --simple-prompt"
    python-shell-prompt-regexp "In \\[[0-9]+\\]: "
    python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
    python-shell-completion-setup-code
    "from IPython.core.completerlib import module_completion"))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package anaconda-mode
  :ensure t
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

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
  "Orgmode setup."
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

(defun mug/org-mode-visual-fill ()
  "Side columns filling to make org mode more presentable."
  (setq visual-fill-column-width 80
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
