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
;;
;; NOTE: Eval the configs with <C-c e> and precompile into bytecode with <C-c C-f>

;;; Code:

;;; ============================================================================
;;; Bytecode compiler settings
;;; ============================================================================

;; Ignore useless compiler warnings that actually don't even concern this init.el
(setq native-comp-async-report-warnings-errors nil)
(remove-hook 'native-comp-async-compile-warnings-hook 'native-comp-async-compile-warnings-logger)

;; Annoying as hell
(setq byte-compile-warnings '(not free-vars unresolved))

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

(defconst mug-home-dir (eval-when-compile (mug-win-or-linux (getenv "USERPROFILE") "~/"))
  "User home directory.")

(defconst mug-dev-dir (eval-when-compile (mug-win-or-linux "d:/" (concat mug-home-dir "/projects")))
  "Development directory.")

(defconst mug-cache-dir (eval-when-compile (mug-win-or-linux (getenv "LOCALAPPDATA") "~/.cache"))
  "User general cache directory.")

(defconst mug-temp-dir (eval-when-compile (mug-win-or-linux (getenv "TEMP") "/tmp"))
  "User temporary directory.")

(defconst mug-emacs-dir (eval-when-compile (concat mug-home-dir "/.config/mugdot/emacs"))
  "Directory where the Emacs configuration resides.")

(defconst mug-emacs-cache-dir (eval-when-compile (concat mug-cache-dir "/emacs"))
  "Directory where Emacs should store its caching.")

(defconst mug-emacs-backup-dir (eval-when-compile (concat mug-emacs-cache-dir "/backups"))
  "Emacs cache directory where backups reside.")

(defconst mug-emacs-temp-dir (eval-when-compile (concat mug-temp-dir "/emacs"))
  "Directory where emacs should store its temporary files.")

;; Directory containing additional modules
(add-to-list 'load-path (concat mug-emacs-dir "/lisp"))

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
  "Load the Emacs init.el file to the current window."
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
  (package-native-compile t)

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
  (make-backup-files      t)         ; Make backups frequently
  (backup-by-copying      t)         ; Copy the files instead of renaming them
  (kept-new-versions     10)         ; Number of new versions to maintain
  (kept-old-versions      3)         ; Number of old versions to maintain
  (delete-old-versions    t)         ; Clean the backup directory
  (version-control        t)         ; Use numbering of the backup files
  (vc-make-backup-files   t)         ; Store even files controlled by a version-control system
  (auto-save-default    nil)         ; We'll use the super-save package

  ;; Ignore the following extensions when completing in the minibuffer
  (completion-ignored-extensions '(".a" ".o" ".so" ".lib" ".elf" ".pdb" ".bin" ".exe" ".elc" ".aux" ".bbl" ".toc" ".blg" "~"))

  ;; Don't delay parenthesis matching
  (show-paren-delay 0)

  (indent-tabs-mode nil) ;; Don't use tabs for indentation
  (tab-width          4) ;; If you gotta render a tab, at least use a resonable width...


  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)

  ;; Horrible default UI stuff.
  (window-divider-mode               -1)
  (window-divider-default-places    nil)
  (column-number-mode                -1)
  (display-line-numbers-mode         -1)
  (mode-line-percent-position       nil)
  (menu-bar-mode                    nil)
  (tool-bar-mode                    nil)
  (scroll-bar-mode                  nil)
  (tooltip-mode                      -1)
  (ring-bell-function           'ignore)
  (visible-bell                     nil)
  (inhibit-startup-message            t)
  (initial-scratch-message          nil)

  ;; Scrolling settings
  (scroll-step             3)
  (scroll-conservatively 101)
  (auto-window-vscroll   nil)

  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  ;; Hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)

  ;; Use UTF-8 encoding for everything
  (dolist (fn '(set-terminal-coding-system
                set-language-environment
                set-keyboard-coding-system
                prefer-coding-system
                locale-coding-system
                set-default-coding-systems))
    (funcall fn 'utf-8))

  :config

  ;; Make accepting commands easier with y/n options
  (defalias 'yes-or-no-p 'y-or-n-p)

  ;; Set the default font
  (set-face-attribute 'default nil
                      :font (mug-win-or-linux "Terminus (TTF) for Windows"
                                              "Terminus")
                      :height 120)

  ;; Window resizing
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  ;; Fringe border
  (set-fringe-mode '(2 . 2))

  ;; <Escape> from anything
  (keymap-global-set "<escape>" 'keyboard-escape-quit)

  ;; Translate <C-k> into an escape key press
  (define-key key-translation-map (kbd "C-k") (kbd "<escape>"))

  ;; Suppress meaningless messages in the *Messages* buffer:
  (dolist (fn '(basic-save-buffer undo undo-redo beginning-of-line end-of-line))
    (advice-add fn :around #'mug--suppress-message))

  ;; Annoying keybindings:
  (dolist (key '("C-x C-b" "C-x C-c" "C-x C-d" "C-x C-z" "C-x C-s" "C-x s" "C-x f" "C-h h"
                 "C-h k" "C-h j" "C-h l" "C-f" "C-z" "C-j" "M-j"))
    (keymap-global-unset key))

  ;; This is needed for me because my keyboard uses the Caps Lock as a "magic" key
  ;; when pressed together with some other key, in order to prevent such a fucking
  ;; bullshit, I translated every possible shit that could happen into the right
  ;; thing that it should be binded in the first place.
  (define-key key-translation-map (kbd "C-<up>")    (kbd "C-w"))
  (define-key key-translation-map (kbd "C-<left>")  (kbd "C-a"))
  (define-key key-translation-map (kbd "C-<down>")  (kbd "C-s"))
  (define-key key-translation-map (kbd "C-<right>") (kbd "C-d")))

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
;; Emacs theme and styling
;; -----------------------------------------------------------------------------

(use-package modus-themes
  :init
  (defconst modus-vivendi-tinted-palette-overrides
    ;; Language construct mappings
    '((variable fg-main)
      (comment  fg-clay)
      (type     fg-active-value)
      (constant fg-active-value)
      (fnname   fg-main)))
  (load-theme 'modus-vivendi-tinted))

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
  :bind (:map vertico-map
              ("C-j" . vertico-previous)
              ("C-n" . vertico-next))
  :custom   (vertico-cycle t))

;;; ============================================================================
;;; Getting EVIL
;;; ============================================================================

(use-package evil
  :defines  (evil-mode evil-emacs-state-modes evil-normal-state-map)
  :commands (evil-yank evil-global-set-key)
  :init
  (setq-default evil-want-keybinding     nil)
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
  :bind (:map evil-normal-state-map
              ("Y"       . (lambda () (interactive) (evil-yank (point) (line-end-position))))
              ("SPC w"   . save-buffer)
              ("SPC q"   . kill-buffer-and-window)
              ("SPC s"   . split-window-right)
              ("SPC h"   . split-window-below)
              ("SPC o"   . other-window)
              ("SPC b b" . switch-to-buffer)
              ("SPC f f" . find-file)
              ("SPC f o" . find-file-other-window)
              ("SPC f p" . project-find-file)
              ("SPC c c" . project-compile)
              ("SPC r r" . project-recompile)
              ("SPC f z" . rg))
  :config
  (defun mug-evil-hook ()
    "My evil mode."
    (dolist (mode '(custom-mode
                    eshell-mode
                    git-rebase-mode
                    term-mode))
      (add-to-list 'evil-emacs-state-modes mode)))
  (add-hook 'evil-mode-hook #'mug-evil-hook)

  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Disable useless messages...
  (dolist (fn '(evil-forward-char evil-backward-char evil-next-line evil-previous-line evil-line-move))
    (advice-add fn :around #'mug--suppress-message))

  ;; Keybinding goodies
  (use-package evil-collection
    :requires evil
    :defines  (evil-collection-mode-list)
    :commands (evil-collection-init evil-collection-define-key)
    :init     (evil-collection-init)
    :custom
    (evil-collection-outline-bind-tab-p nil)
    (evil-collection-want-unimpaired-p  nil)
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

(use-package xref
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
              ("C-c u" . citre-update-this-tags-file))
  :init
  (require 'citre-config)
  :config
  ;; Citre setup
  (setq citre-ctags-program (mug-win-or-linux
                             (concat mug-home-dir "/scoop/apps/universal-ctags/current/ctags.exe")
                             "/usr/bin/ctags")
        citre-tags-global-cache-dir             (concat mug-emacs-cache-dir "/tags")
        citre-default-create-tags-file-location 'in-dir
        citre-auto-enable-citre-mode-modes      '(prog-mode)))

(use-package corfu
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)  ;; Enable auto completion
  :init
  (global-corfu-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  :custom
  (cape-dabbrev-min-length 3))

(use-package yasnippet
  :custom
  (yas-snippet-dirs (concat mug-emacs-dir "/snippets")))

;; Git management with Magit for the win
(straight-use-package 'magit)

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
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.41.34120/include"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/VS/include"
           ";" "C:/Program Files (x86)/Windows Kits/10/include/10.0.22621.0/ucrt"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/um"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/shared"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/winrt"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/cppwinrt"
           ";" "C:/Program Files (x86)/Windows Kits/NETFXSDK/4.8/include/um"))

  (setenv "LIB"
          (concat
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.41.34120/lib/x64"
           ";" "C:/Program Files (x86)/Windows Kits/NETFXSDK/4.8/lib/um/x64"
           ";" "C:/Program Files (x86)/Windows Kits/10/lib/10.0.22621.0/ucrt/x64"
           ";" "C:/Program Files (x86)/Windows Kits/10//lib/10.0.22621.0//um/x64"))

  (setenv  "LIBPATH"
           (concat
            "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.41.34120/lib/x64"
            ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.41.34120/lib/x86/store/references"
            ";" "C:/Program Files (x86)/Windows Kits/10/UnionMetadata/10.0.22621.0"
            ";" "C:/Program Files (x86)/Windows Kits/10/References/10.0.22621.0"
            ";" "C:/Windows/Microsoft.NET/Framework64/v4.0.30319"))

  (setenv "PATH"
          (concat
           (getenv "PATH")
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.41.34120/bin/HostX64/x64"
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
;; C/C++ utility functions
;; -----------------------------------------------------------------------------

(defconst mug-c-formatter "clang-format"
  "The name of the C/C++ formatter executable")

(defconst mug-c-formatter-found (eval-when-compile (executable-find mug-c-formatter))
  "Whether the C/C++ formatter `mug--c-formatter' executable was found or not.")

(defun mug-c-format-buffer ()
  "Format the current buffer."
  (interactive)
  (let ((current-point        (point))
        (current-window-start (window-start)))
    (shell-command-on-region (point-min) (point-max) mug-c-formatter nil t)
    ;; Return the user to the previous state
    (set-window-start (selected-window) current-window-start)
    (goto-char current-point)))

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

(defun mug-c-scratch-buf ()
  "Make a C scratch buffer"
  (interactive)
  (find-file (concat mug-emacs-temp-dir "/scratch.c")))

;; Lightweight C-mode
(require 'simpc-mode)

(add-hook 'simpc-mode-hook (lambda () 'before-save-hook #'mug-c-format-buffer nil t))

;; -----------------------------------------------------------------------------
;; Misc. secondary langs
;; -----------------------------------------------------------------------------

(autoload 'go-mode "go-mode" nil t)

(use-package lua-mode
  :custom
  (lua-indent-level 4))

;; -----------------------------------------------------------------------------
;; General programming setup
;; -----------------------------------------------------------------------------

(setq auto-mode-alist
      (append
       '(("\\.h\\(h\\|pp\\)?\\'"                                . simpc-mode)
         ("\\.c\\(c\\|pp\\)?\\'"                                . simpc-mode)
         ("\\.\\(?:comp\\|vert\\|geom\\|frag\\|tesc\\|tese\\)$" . simpc-mode)
         ("\\.go$"                                              . go-mode)
         ("\\.lua$"                                             . lua-mode)
         ("\\.txt$"                                             . indented-text-mode))
       auto-mode-alist))

(require 'ansi-color)

(defun mug--colourful-compilation ()
  "Handle ansi escape sequences from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'mug--colourful-compilation)

(use-package prog-mode
  :straight (:type built-in)
  :hook ((prog-mode . yas-minor-mode)
         (prog-mode . citre-mode))
  :bind (:map evil-normal-state-map
              ("g d"     . citre-jump)
              ("g b"     . citre-jump-back)
              ("g o"     . xref-find-definitions-other-window)
              ("SPC f c" . mug-c-find-corresponding)
              ("SPC f b" . mug-c-format-buffer)))

;; -----------------------------------------------------------------------------

(provide 'init)

;;; init.el ends here
