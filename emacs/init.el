;;; init.el --- Emacs configuration file --- -*- lexical-binding: t; -*-

;;; Commentary:

;; My custom init file for GNU Emacs.
;; Author: Luiz G. Mugnaini A. <luizmugnaini@gmail.com>
;;
;;
;;
;;                         .,,uod8B8bou,,.
;;                ..,uod8BBBBBBBBBBBBBBBBRPFT?l!i:.
;;           ,=m8BBBBBBBBBBBBBBBRPFT?!||||||||||||||
;;           !...:!TVBBBRPFT||||||||||!!^^""'   ||||
;;           !.......:!?|||||!!^^""'            ||||
;;           !.........||||                     ||||
;;           !.........||||  ##                 ||||
;;           !.........||||                     ||||
;;           !.........||||                     ||||
;;           !.........||||                     ||||
;;           !.........||||                     ||||
;;           `.........||||                    ,||||
;;            .;.......||||               _.-!!|||||
;;     .,uodWBBBBb.....||||       _.-!!|||||||||!:'
;;  !YBBBBBBBBBBBBBBb..!|||:..-!!|||||||!iof68BBBBBb....
;;  !..YBBBBBBBBBBBBBBb!!||||||||!iof68BBBBBBRPFT?!::   `.
;;  !....YBBBBBBBBBBBBBBbaaitf68BBBBBBRPFT?!:::::::::     `.
;;  !......YBBBBBBBBBBBBBBBBBBBRPFT?!::::::;:!^"`;:::       `.
;;  !........YBBBBBBBBBBRPFT?!::::::::::^''...::::::;         iBBbo.
;;  `..........YBRPFT?!::::::::::::::::::::::::;iof68bo.      WBBBBbo.
;;    `..........:::::::::::::::::::::::;iof688888888888b.      YBBBP^
;;      `........::::::::::::::::;iof688888888888888888888b.
;;        `......:::::::::;iof688888888888888888888888888888b.
;;          `....:::;iof688888888888888888888888888888888899fT!
;;            `..::!8888888888888888888888888888888899fT|!^"'
;;              `' !!988888888888888888888888899fT|!^"'
;;                  `!!8888888888888888899fT|!^"'
;;                    `!988888888899fT|!^"'
;;                      `!9899fT|!^"'
;;                        `!^"'
;;
;;
;; (I don't know who made this ascii art)

;;; Code:

;;; ============================================================================
;;; Startup
;;; ============================================================================

(defconst mug-emacs-start-time (current-time))

;; Avoid garbage collection at startup.
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 128 1024 1024))

(defun mug-display-startup-time ()
  "Display the Emacs startup time."
  (message "Emacs loaded in %s with %d garbage collections."
	   (format "%.2f seconds"
		   (float-time
		    (time-subtract (current-time) mug-emacs-start-time)))
	   gcs-done))

(add-hook 'after-init-hook #'mug-display-startup-time) ; Report startup time
(add-hook 'after-init-hook #'garbage-collect t)        ; Restart garbage collection after startup

;;; ============================================================================
;;; System information
;;; ============================================================================

(defconst mug-sys-is-win (eval-when-compile (eq system-type 'windows-nt))
  "Indicates if the current system is Windows or not.")
(defconst mug-path-env   (getenv "PATH")
  "The PATH environment variable at the time of initialization.")

(defun mug-win-or-linux (win-option linux-option)
  "Choose between WIN-OPTION and LINUX-OPTION options."
  (if mug-sys-is-win
      win-option
    linux-option))

;;; ============================================================================
;;; Directories
;;; ============================================================================

;; -----------------------------------------------------------------------------
;; User directories
;; -----------------------------------------------------------------------------

(defconst mug-home-dir (eval-when-compile (mug-win-or-linux (getenv "USERPROFILE") "~/"))
  "User home directory.")

(defconst mug-dev-dir (eval-when-compile (mug-win-or-linux "d:/" (concat mug-home-dir "/Projects")))
  "Development directory.")

(defconst mug-cache-dir (eval-when-compile (mug-win-or-linux (getenv "LOCALAPPDATA") "~/.cache"))
  "User general cache directory.")

(defconst mug-temp-dir (eval-when-compile (mug-win-or-linux (getenv "TEMP") "/tmp"))
  "User temporary directory.")

;; -----------------------------------------------------------------------------
;; Directories to be used by Emacs
;; -----------------------------------------------------------------------------

(defconst mug-emacs-dir (eval-when-compile (concat mug-home-dir "/.config/mugdot/emacs"))
  "Directory where the Emacs configuration resides.")

(defconst mug-emacs-cache-dir (eval-when-compile (concat mug-cache-dir "/emacs"))
  "Directory where Emacs should store its caching.")

(defconst mug-emacs-backup-dir (eval-when-compile (concat mug-emacs-cache-dir "/backups"))
  "Emacs cache directory where backups reside.")

(defconst mug-emacs-temp-dir (eval-when-compile (concat mug-temp-dir "/emacs"))
  "Directory where emacs should store its temporary files.")

;; Set the default directories
(setq default-directory    mug-dev-dir
      custom-file          (eval-when-compile (concat mug-emacs-dir "/custom.el"))
      user-emacs-directory mug-emacs-dir)

;; Create the caching directories if non-existent
(if (not (file-exists-p mug-emacs-cache-dir))
    (make-directory mug-emacs-cache-dir))
(if (not (file-exists-p mug-emacs-backup-dir))
    (make-directory mug-emacs-backup-dir))

;;; ============================================================================
;;; Post initialization
;;; ============================================================================

(defun display-startup-echo-area-message ()
  "Substitutes the default function in order to disable startup messages.")

(defun mug--emacs-post-init-hook ()
  "Initialize emacs the way I want."
  (kill-buffer "*scratch*")
  (find-file mug-dev-dir))

(add-hook 'after-init-hook #'mug--emacs-post-init-hook)

;;; ============================================================================
;;; Misc helper functions
;;; ============================================================================

(defun mug--suppress-message (orig-fun &rest args)
  "Suppress message in the *Messages* buffer."
  (let ((inhibit-message t))
    (apply orig-fun args)))

(defun mug-open-file-to-the-right (filename)
  "Open FILENAME in a new window to the right."
  (interactive "Open file to the right: ")
  (let ((new-window (split-window-right)))
    (select-window new-window)
    (find-file filename)))

(defun mug-find-emacs-config ()
  "Load the Emacs init.el file to the current frame."
  (interactive)
  (find-file (concat mug-emacs-dir "/init.el")))

;;; ============================================================================
;;; straight.el package manager bootstrapping
;;; ============================================================================

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

;;; ============================================================================
;;; Emacs global setup
;;; ============================================================================

(use-package emacs
  :hook
  (before-save      . delete-trailing-whitespace)
  (minibuffer-setup . cursor-intangible-mode)
  :custom
  ;; Allow foreign themes without asking
  (custom-safe-themes t)

  ;; Prefer opening the latest version of the file
  (load-prefer-newer t)

  ;; Stop Emacs from losing undo information by setting very high
  ;; limits for undo buffers
  (undo-limit        20000000)
  (undo-strong-limit 40000000)

  ;; Put a low limit on the *Messages* buffer
  (message-log-max 10)

  ;; Setup Emacs backups and auto-saves
  (backup-directory-alist
   `(("." . ,mug-emacs-backup-dir))) ; Set the backup directory
  (make-backup-files    t)           ; Make backups frequently
  (backup-by-copying    t)           ; Copy the files instead of renaming them
  (kept-new-versions    10)          ; Number of new versions to maintain
  (kept-old-versions    3)           ; Number of old versions to maintain
  (delete-old-versions  t)           ; Clean the backup directory
  (version-control      t)           ; Use numbering of the backup files
  (vc-make-backup-files t)           ; Store even files controlled by a version-control system
  (auto-save-default    nil)         ; We'll use the super-save package

  ;; Ignore the following extensions when completing in the minibuffer
  (completion-ignored-extensions
   '(".a"
     ".o"
     ".so"
     ".elf"
     ".pdb"
     ".bin"
     ".exe"
     ".elc"
     ".aux"
     ".bbl"
     ".toc"
     ".blg"
     "~"))

  ;; Don't delay parenthesis matching
  (show-paren-delay 0)

  ;; Don't use tabs for indentation
  (indent-tabs-mode nil)

  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)

  ;; Horrible default UI stuff.
  (window-divider-mode           -1)
  (window-divider-default-places nil)
  (column-number-mode            -1)
  (display-line-numbers-mode     -1)
  (mode-line-percent-position    nil)
  (menu-bar-mode                 nil)
  (tool-bar-mode                 nil)
  (scroll-bar-mode               nil)
  (tooltip-mode                  -1)
  (ring-bell-function            'ignore)
  (visible-bell                  nil)
  (inhibit-startup-message       t)
  (initial-scratch-message       nil)

  ;; Scrolling settings
  (scroll-step           3)
  (scroll-conservatively 101)
  (auto-window-vscroll   nil)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Use UTF-8 encoding for everything
  (set-terminal-coding-system 'utf-8)
  (set-language-environment   'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system       'utf-8)
  (locale-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  :config

  ;; Make accepting commands easier with y/n options
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Set the default font
  (set-face-attribute 'default nil
                      :font (mug-win-or-linux "Terminus (TTF) for Windows"
                                              "Terminus (TTF)")
                      :height 120)

  ;; Window resizing
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Fringe border
  (set-fringe-mode               '(2 . 2))

  ;; <Escape> from anything
  (keymap-global-set "<escape>" 'keyboard-escape-quit)

  ;; Translate <C-k> into an escape key press
  (define-key key-translation-map (kbd "C-k") (kbd "<escape>"))

  ;; Simplify the auto mode-detection
  (setq auto-mode-alist
        (append
         '(("\\.c$"    . c-mode)
           ("\\.cc$"   . c++-mode)
           ("\\.cpp$"  . c++-mode)
           ("\\.h$"    . c++-mode) ; oof, C++?
           ("\\.hh$"   . c++-mode)
           ("\\.hpp$"  . c++-mode)
           ;; Use GLSL as if it was C
           ("\\.glsl$" . c-mode)
           ("\\.comp$" . c-mode)
           ("\\.vert$" . c-mode)
           ("\\.geom$" . c-mode)
           ("\\.frag$" . c-mode)
           ("\\.tesc$" . c-mode)
           ("\\.tese$" . c-mode)
           ;; Ordinary text files
           ("\\.txt$"  . indented-text-mode)) auto-mode-alist))

  ;; Suppress meaningless messages in the *Messages* buffer:
  (advice-add 'basic-save-buffer :around #'mug--suppress-message)
  (advice-add 'undo              :around #'mug--suppress-message)
  (advice-add 'undo-redo         :around #'mug--suppress-message)
  (advice-add 'beginning-of-line :around #'mug--suppress-message)
  (advice-add 'end-of-line       :around #'mug--suppress-message)

  ;; Annoying keyindings:
  ;; Stuff that I keep pressing and doing something terrible or annoying, or that
  ;; I want to use for something else.
  (keymap-global-unset "C-x C-b")
  (keymap-global-unset "C-x C-c")
  (keymap-global-unset "C-x C-d")
  (keymap-global-unset "C-x C-z")
  (keymap-global-unset   "C-x s")
  (keymap-global-unset   "C-x f")
  (keymap-global-unset   "C-h h")
  (keymap-global-unset   "C-h k")
  (keymap-global-unset   "C-h j")
  (keymap-global-unset   "C-h l")
  (keymap-global-unset     "C-f")
  (keymap-global-unset     "C-z")
  (keymap-global-unset     "C-j")
  (keymap-global-unset     "M-j")

  ;; This is needed for me because my keyboard uses the Caps Lock as a "magic" key
  ;; when pressed together with some other key, in order to prevent such a fucking
  ;; bullshit, I translated every possible shit that could happen into the right
  ;; thing that it should be binded in the first place.
  (define-key key-translation-map (kbd "C-<up>")           (kbd "C-w"))
  (define-key key-translation-map (kbd "C-<left>")         (kbd "C-a"))
  (define-key key-translation-map (kbd "C-<down>")         (kbd "C-s"))
  (define-key key-translation-map (kbd "C-<right>")        (kbd "C-d"))
  (define-key key-translation-map (kbd "C-<print>")        (kbd "C-p"))
  (define-key key-translation-map (kbd "C-<prt-scr>")      (kbd "C-p"))
  (define-key key-translation-map (kbd "C-<print-screen>") (kbd "C-p")))

;; -----------------------------------------------------------------------------
;; Elisp setup
;; -----------------------------------------------------------------------------

;; Evaluate the entirety of the current buffer
(add-hook 'emacs-lisp-mode-hook
	  (lambda () (local-set-key (kbd "C-c e") 'eval-buffer)))

;; -----------------------------------------------------------------------------
;; Dired setup
;; -----------------------------------------------------------------------------

(use-package dired
  :straight (:type built-in)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  :init
  (put 'dired-find-alternate-file 'disabled nil)

  ;; Helper packages for a better dired experience
  (straight-use-package 'dired-single)
  (straight-use-package 'dired-collapse))

;; -----------------------------------------------------------------------------
;; Automatic backup management
;; -----------------------------------------------------------------------------

(use-package super-save
  :defines (super-save-mode super-save-hook-triggers)
  :init
  (super-save-mode +1)
  :custom
  (super-save-silent              t)
  (super-save-auto-save-when-idle t)
  :config
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook))

;; -----------------------------------------------------------------------------
;; Emacs theme and styling
;; -----------------------------------------------------------------------------

(use-package modus-themes
  :init (load-theme 'modus-vivendi-deuteranopia))

;; -----------------------------------------------------------------------------
;; General utilities
;; -----------------------------------------------------------------------------

;; Better grep alternative
(defconst mug--ripgrep-available (eval-when-compile (executable-find "rg"))
  "Whether the program Ripgrep is available or not.")
(if mug--ripgrep-available
    (straight-use-package 'rg))

;; Mini-buffer completion
(use-package vertico
  :commands (vertico-mode)
  :init     (vertico-mode)
  :custom   (vertico-cycle t))


;;; ============================================================================
;;; Getting EVIL
;;; ============================================================================

(use-package evil
  :defines  (evil-mode evil-emacs-state-modes evil-normal-state-map)
  :commands (evil-yank evil-global-set-key)
  :init
  (setq-default evil-want-keybinding nil)
  (setq evil-want-integration              t
	evil-want-C-u-scroll                 nil
	evil-want-C-i-jump                   nil
	evil-respect-visual-line-mode          t
	evil-undo-system              'undo-redo
	evil-shift-width                       4
	evil-insert-state-cursor            'box
	evil-normal-state-cursor            'box
	evil-esc-delay                         0)

  (evil-mode 1)
  :config
  (defun mug-evil-hook ()
    "My evil mode."
    (dolist (mode '(custom-mode
                    eshell-mode
                    git-rebase-mode
                    term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))
  (add-hook 'evil-mode-hook #'mug-evil-hook)

  (defun mug-evil-yank-to-end-of-line ()
    "Yank to end of line."
    (interactive)
    (evil-yank (point) (line-end-position)))

  ;; Keybindings
  (define-key evil-normal-state-map (kbd "Y") 'mug-evil-yank-to-end-of-line)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable useless messages...
  (advice-add 'evil-forward-char  :around #'mug--suppress-message)
  (advice-add 'evil-backward-char :around #'mug--suppress-message)
  (advice-add 'evil-next-line     :around #'mug--suppress-message)
  (advice-add 'evil-previous-line :around #'mug--suppress-message)
  (advice-add 'evil-line-move     :around #'mug--suppress-message)

  ;; Keybinding goodies
  (use-package evil-collection
    :requires evil
    :defines  (evil-collection-mode-list)
    :commands (evil-collection-init evil-collection-define-key)
    :init     (evil-collection-init)
    :custom
    (evil-collection-outline-bind-tab-p nil)
    :config
    (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))

    (evil-collection-define-key 'normal 'dired-mode-map
      "h" (lambda () (interactive) (find-alternate-file ".."))
      "H" 'dired-omit-mode
      "l" 'dired-single-buffer
      "y" 'dired-ranger-copy
      "X" 'dired-ranger-move
      "p" 'dired-ranger-paste))

  ;; Commenting made easier
  (use-package evil-nerd-commenter
    :bind ("M-;" . evilnc-comment-or-uncomment-lines)))

;;; ============================================================================
;;; General development setup
;;; ============================================================================

;; Git management with Magit for the win
(straight-use-package 'magit)

(use-package prog-mode
  :straight (:type built-in)
  :bind (:map prog-mode-map
              ("C-p c" . project-compile)
              ("C-p r" . project-recompile)
              ("C-p f" . project-find-file))
  :config
  ;; Automatic bracket pairing
  (electric-pair-mode 1))

;; -----------------------------------------------------------------------------
;; Completions, tags, and jumping around!
;; -----------------------------------------------------------------------------

(use-package xref
  :defines (xref-show-definitions-function)
  :bind (:map prog-mode-map
              ("C-j s" . xref-find-apropos))
  :custom
  ;; Show results in the minibuffer
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function       #'xref-show-definitions-completing-read)
  :config
  ;; Use ripgrep when available
  (if mug--ripgrep-available
    (setq xref-search-program 'ripgrep)))

(use-package citre
  :defer t
  :bind (:map citre-mode-map
              ("C-c j" . citre-jump)
              ("C-c J" . citre-jump-back)
              ("C-c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  :config
  (setq citre-ctags-program (mug-win-or-linux
                             (concat mug-home-dir "/scoop/apps/universal-ctags/current/ctags.exe")
                             "/usr/bin/ctags")
        citre-tags-global-cache-dir             (concat mug-emacs-cache-dir "/tags")
        citre-default-create-tags-file-location 'in-dir
        citre-auto-enable-citre-mode-modes      '(prog-mode)))

(use-package company
  :defines  (company-active-map)
  :commands (company-complete company-select-previous company-select-next)
  :hook     (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-m" . company-select-previous)
              ("C-n" . company-select-next))
  :custom
  (company-dabbrev-other-buffers      t)
  (company-dabbrev-code-other-buffers t)
  (company-show-numbers               nil)
  (company-require-match              nil) ; Don't mess with my key presses!!
  (company-minimum-prefix-length        1) ; Number of characters needed before starting the search
  (company-idle-delay                 0.1) ; Delay before starting to search
  :config
  (setq company-backends '((company-capf
                            company-clang
                            company-files
                            company-semantic
                            :with company-yasnippet))))

;; -----------------------------------------------------------------------------
;; Snippets
;; -----------------------------------------------------------------------------

(use-package yasnippet
  :hook ((LaTeX-hook . yas-minor-mode)
         (prog-mode  . yas-minor-mode))
  :custom
  (yas-snippet-dirs (concat mug-emacs-dir "/snippets")))

;; -----------------------------------------------------------------------------
;; Colours in compilation mode
;; -----------------------------------------------------------------------------

(require 'ansi-color)
(defun mug-colourful-compilation ()
  "Handle ansi escape sequences from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'mug-colourful-compilation)

;;; ============================================================================
;;; C and C++ setup
;;; ============================================================================

;; -----------------------------------------------------------------------------
;; Handle the Windows MSVC environment hell
;; -----------------------------------------------------------------------------

(defun mug-c-msvc-toolchain ()
  "Set MSVC toolchain environment variables.

This is extremely non-portable so my tip for working with this shitty
Windows environment is: open a decent shell, run `vcvarsall.bat x64`
there and copy the results coming from `echo %[INCLUDE | LIB | LIBPATH | PATH]%`
that are relevant for your installation. "
  (interactive)
  (message "Setting 64 bits building tools.")
  (setenv "PATH" mug-path-env)
  (setenv "INCLUDE"
          (concat
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.39.33519/include"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/VS/include"
           ";" "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22621.0/ucrt"
           ";" "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22621.0/um"
           ";" "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22621.0/shared"
           ";" "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22621.0/winrt"
           ";" "C:/Program Files (x86)/Windows Kits/10/Include/10.0.22621.0/cppwinrt"
           ))

  (setenv "LIB"
          (concat
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.39.33519/lib/x64"
           ";" "C:/Program Files (x86)/Windows Kits/10/Lib/10.0.22621.0/ucrt/x64"
           ";" "C:/Program Files (x86)/Windows Kits/10/Lib/10.0.22621.0/um/x64"
           ))

  (setenv  "LIBPATH"
           (concat
            "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.39.33519/lib/x64"
            ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.39.33519/lib/x86/store/references"
            ";" "C:/Program Files (x86)/Windows Kits/10/UnionMetadata/10.0.22621.0"
            ";" "C:/Program Files (x86)/Windows Kits/10/References/10.0.22621.0"
            ";" "C:/Windows/Microsoft.NET/Framework64/v4.0.30319"
            ))

  (setenv "PATH"
          (concat
           (getenv "PATH")
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.39.33519/bin/HostX64/x64"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/VC/VCPackages"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/CommonExtensions/Microsoft/TestWindow"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/CommonExtensions/Microsoft/TeamFoundation/Team Explorer"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/MSBuild/Current/bin/Roslyn"
           ";" "C:/Program Files (x86)/Windows Kits/10/bin/10.0.22621.0//x64"
           ";" "C:/Program Files (x86)/Windows Kits/10/bin//x64"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community//MSBuild/Current/Bin/amd64"
           ";" "C:/Windows/Microsoft.NET/Framework64/v4.0.30319"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/IDE/"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/Common7/Tools/")))

;; -----------------------------------------------------------------------------
;; Formatting C/C++ and C-like files:
;; -----------------------------------------------------------------------------

(defconst mug-c-formatter "clang-format"
  "The name of the C/C++ formatter executable")

(defconst mug-c-formatter-found (eval-when-compile (executable-find mug-c-formatter))
  "Whether the C/C++ formatter `mug-c-formatter' executable was found or not.")

(defun mug-c-format-buffer ()
  "Format the current buffer."
  (interactive)
  (shell-command-on-region (point-min) (point-max) mug-c-formatter nil t))

;; -----------------------------------------------------------------------------
;; Find header or implementation of the current C/C++ file
;; -----------------------------------------------------------------------------

;; TODO: support project include/ directories, not only same-directory structures
(defun mug--c-find-impl-or-decl-file-name ()
  "Find name of the implementation or header of the current buffer."
  (let ((base-file-name (file-name-sans-extension buffer-file-name)))
    (cond
     ;; In case of an implementationfile, we search for the header
     ((string-match "\\.c" buffer-file-name)   (concat base-file-name ".h"))
     ((string-match "\\.cc" buffer-file-name)  (concat base-file-name ".h"))
     ((string-match "\\.cpp" buffer-file-name) (concat base-file-name ".h"))
     ;; In case of a header, we search for the implementation
     ((string-match "\\.h" buffer-file-name)
      (cond
       ((file-exists-p (concat base-file-name ".c"))    (concat base-file-name ".c"))
       ((file-exists-p (concat base-file-name ".cc"))   (concat base-file-name ".cc"))
       ((file-exists-p (concat base-file-name ".cpp"))  (concat base-file-name ".cpp")))))))

(defun mug-c-find-corresponding ()
  "Find the impl/header of the current buffer and open in the current window."
  (interactive)
  (let ((file-name (mug--c-find-impl-or-decl-file-name)))
    (if file-name
        (find-file file-name)
      (error "Unable to find a file corresponding to %s" buffer-file-name))))

(defun mug-c-find-corresponding-other-window ()
  "Find the impl/header of the current buffer and open in the adjacent window."
  (interactive)
  (let ((file-name (mug--c-find-impl-or-decl-file-name)))
    (if file-name
        (find-file-other-window file-name)
      (error "Unable to find a file corresponding to %s" buffer-file-name))))

;; -----------------------------------------------------------------------------
;; Just like elisp, we should also have scratch buffers for C/C++
;; -----------------------------------------------------------------------------

(defun mug-c-scratch-buf ()
  "Make a C++ scratch buffer"
  (interactive)
  (find-file (concat mug-emacs-temp-dir "/scratch.cc")))

;; -----------------------------------------------------------------------------
;; C and C++ styling and keybindings
;; -----------------------------------------------------------------------------

(defun mug-c-mode-hook ()
  "Set C and C++ styling"
  ;; Indentation width
  (setq tab-width 4)

  ;; Auto-format the buffer before saving if the `mug-c-formatter' is found.
  (if mug-c-formatter-found
      (add-hook 'before-save-hook #'mug-c-format-buffer nil 'local)))

(use-package cc-mode
  :straight (:type built-in)
  :bind
  (:map c++-mode-map
        ("C-c f" . mug-c-find-corresponding))
  (:map c-mode-map
        ("C-c f" . mug-c-find-corresponding))
  :config
  (add-hook 'c++-mode-hook  #'mug-c-mode-hook)
  (add-hook 'c-mode-hook    #'mug-c-mode-hook))

;;; ============================================================================
;;; Latex configuration:
;;; ============================================================================

;; (use-package latex
;;   :defer t
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
;;   (when mug-sys-is-win
;;     (setq exec-path (append exec-path '(concat mug-home-dir "/AppData/Local/Programs/MiKTeX/miktex/bin/x64"))))

;;   ;; Basic settings
;;   (setq TeX-parse-self t)
;;   (setq-default TeX-master nil)
;;   (setq visual-fill-column-center-text t)

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
;;            (cdlatex-tab . mug-cdlatex-in-yas-field))
;;     :bind (:map cdlatex-mode-map
;; 		("<tab>" . cdlatex-tab))
;;     :config
;;     (keymap-local-unset "^")
;;     (setq cdlatex-use-dollar-to-ensure-math 0 ;; disable the use of dollar signs
;; 	  cdlatex-paired-parens "{(["
;; 	  cdlatex-simplify-sub-super-scripts t)

;;     ;; YaSnippet integration
;;     (defun mug-cdlatex-in-yas-field ()
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
;;       :commands (aas-set-snippets)
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

;;; init.el ends here
