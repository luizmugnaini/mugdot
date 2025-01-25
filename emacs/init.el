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

;;; ----------------------------------------------------------------------------
;;; Startup
;;; ----------------------------------------------------------------------------

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

;;; ----------------------------------------------------------------------------
;;; System information
;;; ----------------------------------------------------------------------------

(defconst mug-sys-is-win (eval-when-compile (eq system-type 'windows-nt))
  "Indicates if the current system is Windows or not.")

(defun mug-win-or-linux (win-option linux-option)
  "Choose between WIN-OPTION and LINUX-OPTION options."
  (if mug-sys-is-win
      win-option
    linux-option))

(defconst mug-path-env (getenv "PATH")
  "The PATH environment variable at the time of initialization.")

;; Main system directories
(defconst mug-home-dir (eval-when-compile (mug-win-or-linux (getenv "USERPROFILE") "~/"))
  "User home directory.")
(defconst mug-dev-dir (eval-when-compile (mug-win-or-linux "d:/" (concat mug-home-dir "/projects")))
  "Development directory.")

;; Add scoop-installed binaries.
(if mug-sys-is-win
    (dolist (bin-dir '(list (concat mug-home-dir "scoop/shims") "d:/app/bin"))
      (add-to-list 'exec-path bin-dir)))

;; Auxiliar system directories
(defconst mug-cache-dir (eval-when-compile (mug-win-or-linux (getenv "LOCALAPPDATA") "~/.cache"))
  "User general cache directory.")
(defconst mug-temp-dir (eval-when-compile (mug-win-or-linux (getenv "TEMP") "/tmp"))
  "User temporary directory.")

;; Config directories
(defconst mug-mugdot-dir (concat mug-home-dir "/.config/mugdot")
  "Directory where the Emacs configuration resides.")
(defconst mug-emacs-dir (concat mug-mugdot-dir "/emacs")
  "Directory where the Emacs configuration resides.")

;; Functions for navigating to the config directories and files
(defun mug-find-mugdot ()
  "Find dot-files directory."
  (interactive)
  (find-file mug-mugdot-dir))
(defun mug-find-emacs-config ()
  "Load the Emacs init.el file to the current window."
  (interactive)
  (find-file (concat mug-emacs-dir "/init.el")))

;; Specific cache directories
(defconst mug-emacs-cache-dir (concat mug-cache-dir "/emacs")
  "Directory where Emacs should store its caching.")
(defconst mug-emacs-backup-dir (concat mug-emacs-cache-dir "/backups")
  "Emacs cache directory where backups reside.")
(defconst mug-emacs-autosave-dir (concat mug-emacs-cache-dir "/autosave")
  "Emacs cache directory where automatically saved files reside.")
(defconst mug-emacs-temp-dir (concat mug-temp-dir "/emacs")
  "Directory where emacs should store its temporary files.")

;; Create the caching directories if non-existent
(dolist (dir (list mug-emacs-cache-dir mug-emacs-backup-dir mug-emacs-autosave-dir mug-emacs-temp-dir))
  (unless (file-exists-p dir)
    (make-directory dir t)))

;; Directory containing additional modules
(add-to-list 'load-path (concat mug-emacs-dir "/lisp"))

;; Set the default directories
(setq default-directory    mug-dev-dir
      custom-file          (expand-file-name "custom.el" mug-emacs-dir)
      user-emacs-directory mug-emacs-dir)

;;; ----------------------------------------------------------------------------
;;; Misc helper functions
;;; ----------------------------------------------------------------------------

(defun mug-open-file-to-the-right (filename)
  "Open FILENAME in a new window to the right."
  (interactive "Open file to the right: ")
  (let ((new-window (split-window-right)))
    (select-window new-window)
    (find-file filename)))

;;; ----------------------------------------------------------------------------
;;; UI/UX
;;; ----------------------------------------------------------------------------

;; HACK: Avoid calling `menu-bar-mode', `tool-bar-mode', and
;;   `scroll-bar-mode' because their manipulation of frame parameters
;;   can trigger/queue a superfluous frame redraw at startup.
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(setq menu-bar-mode             nil
      tool-bar-mode             nil
      scroll-bar-mode           nil
      tooltip-mode               -1
      column-number-mode         -1
      display-line-numbers-mode  -1)

;; Suppress meaningless messages in the *Messages* buffer:
(dolist (fn '(basic-save-buffer undo undo-redo beginning-of-line end-of-line))
  (advice-add fn :around #'mug--suppress-message))

;; Reduce the clutter in the fringes
(setq indicate-buffer-boundaries nil
      indicate-empty-lines       nil)
(set-fringe-mode '(2 . 2))

;; Disable irrelevant stuff
(setq inhibit-startup-screen       t
      inhibit-startup-message      t
      initial-scratch-message    nil
      mode-line-percent-position nil
      visible-bell               nil)

(defun mug--suppress-message (orig-fun &rest args)
  "Suppress message in the *Messages* buffer."
  (let ((inhibit-message t))
    (apply orig-fun args)))
;; Put a low limit on the `*Messages*' buffer
(setq message-log-max 10)

;; Don't resize the frames in steps; it looks weird, especially in tiling window
;; managers, where it can leave unseemly gaps.
(setq frame-resize-pixelwise t)

;; Don't resize windows pixelwise, this can cause crashes in some cases
;; when resizing too many windows at once or rapidly.
(setq window-resize-pixelwise nil)

;; Prefer fullscreen
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; The native border "consumes" a pixel of the fringe on righter-most
;; splits, `window-divider' does not.
(setq window-divider-default-places       t
      window-divider-default-bottom-width 1
      window-divider-default-right-width  1
      window-divider-mode                 t)

;; Minibuffer tweaks
(setq enable-recursive-minibuffers    t
      minibuffer-prompt-properties    '(read-only t cursor-intangible t face minibuffer-prompt)
      completion-ignored-extensions   '(".a" ".o" ".so" ".lib" ".elf" ".pdb" ".bin" ".exe" ".elc" ".aux" ".bbl" ".toc" ".blg" "~")
      ;; Hide commands in M-x which do not work in the current mode.
      read-extended-command-predicate #'command-completion-default-include-p)

;; Inhibit the use of system based dialog boxes.
(setq use-dialog-box nil)

;; Cursor config
(blink-cursor-mode -1) ; Stop the blinky-blink stuff
(setq show-paren-delay       0  ; Don't delay when trying to find a matching parenthesis
      blink-matching-paren nil  ; Don't be fucking annoying
      x-stretch-cursor     nil) ; Don't stretch the cursor to fit wide characters

;; Don't wrap the lines!!
(set-default 'truncate-lines t)

;; Leaner scrolling experience - like... for real, it is smooooth
(setq scroll-step             3
      scroll-margin           0
      scroll-conservatively 101
      auto-window-vscroll   nil)
(require 'ultra-scroll)
(ultra-scroll-mode 1)

;; Always use UTF-8 encoding
(set-language-environment "UTF-8")

;; Prefer opening the latest version of the file
(setq load-prefer-newer t)

;; Code indentation
(setq indent-tabs-mode nil ; Don't use tabs for indentation
      tab-width        4)  ; If you gotta render a tab, at least use a resonable width...

;; Make it easier to answer stuff.
(if (boundp 'use-short-answers)
    (setq use-short-answers t))
(define-key y-or-n-p-map " " nil) ; Disable `SPC' as a `yes' alias.

;;; ----------------------------------------------------------------------------
;;; Performance hacks
;;; ----------------------------------------------------------------------------

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows.
(setq inhibit-compacting-font-caches t)

;; Performance on Windows is considerably worse than elsewhere. We'll need
;; everything we can get.
(when (boundp 'w32-get-true-file-attributes)
  (setq w32-get-true-file-attributes nil           ; decrease file IO workload
        w32-pipe-read-delay          0             ; faster IPC
        w32-pipe-buffer-size         (* 64 1024))) ; read more at a time (was 4K)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate syntax highlighting right after scrolling, which should
;; quickly self-correct.
(setq fast-but-imprecise-scrolling t)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether disabling it has a notable affect on Linux and Mac
;; hasn't been determined, but do it anyway, just in case. This increases memory
;; usage, however!
(setq inhibit-compacting-font-caches t)

;; Introduced in Emacs HEAD (b2f8c9f), this inhibits fontification while
;; receiving input, which should help a little with scrolling performance.
(setq redisplay-skip-fontification-on-input t)

;; The GC introduces annoying pauses and stuttering into our Emacs experience,
;; so we use `gcmh' to stave off the GC while we're using Emacs, and provoke it
;; when it's idle. However, if the idle delay is too long, we run the risk of
;; runaway memory usage in busy sessions. And if it's too low, then we may as
;; well not be using gcmh at all.
(require 'gcmh)
(gcmh-mode 1)
(setq gcmh-idle-delay             'auto             ; The default is 15s
      gcmh-auto-idle-delay-factor 10
      gcmh-high-cons-threshold    (* 64 1024 1024)) ; 64mb


;;; ----------------------------------------------------------------------------
;;; Backup/history config
;;; ----------------------------------------------------------------------------

;; Stop Emacs from losing undo information by setting very high
;; limits for undo buffers
(setq undo-limit        20000000
      undo-strong-limit 40000000)

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
  '(backup-directory-alist '((".*" . ,mug-emacs-backup-dir))))

;; Setup Emacs backups and auto-saves
(setq backup-directory-alist '(,mug-emacs-backup-dir) ; Set the backup directory
      make-backup-files       t  ; Make backups frequently
      backup-by-copying       t  ; Copy the files instead of renaming them
      kept-new-versions      10  ; Number of new versions to maintain
      kept-old-versions       1  ; Number of old versions to maintain
      delete-old-versions     t  ; Clean the backup directory
      version-control         t  ; Use numbering of the backup files
      vc-make-backup-files    t) ; Store even files controlled by a version-control system

;;; ----------------------------------------------------------------------------
;;; Post initialization
;;; ----------------------------------------------------------------------------

(defun display-startup-echo-area-message ()
  "Substitutes the default function in order to disable startup messages.")

(defun mug--emacs-post-init-hook ()
  "Initialize emacs the way I want."
  (kill-buffer "*scratch*")
  (find-file mug-dev-dir))

(add-hook 'after-init-hook #'mug--emacs-post-init-hook)

;;; ----------------------------------------------------------------------------
;;; Elisp settings
;;; ----------------------------------------------------------------------------

;; Ignore useless compiler warnings that actually don't even concern this init.el
(setq native-comp-async-report-warnings-errors nil)
(remove-hook 'native-comp-async-compile-warnings-hook 'native-comp-async-compile-warnings-logger)

;; Annoying as hell
(setq byte-compile-warnings '(not free-vars unresolved))

;;; ----------------------------------------------------------------------------
;;; straight.el package manager bootstrapping
;;; ----------------------------------------------------------------------------

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

;; Set the builtin `straight.el' integration with `use-package'
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Compile packages for better performance.
(setq package-native-compile t)

;;; -----------------------------------------------------------------------------
;;; Asynchronous capabilities
;;; -----------------------------------------------------------------------------

(straight-use-package 'async)

;; Byte-compile packages asynchronously
(async-bytecomp-package-mode 1)

;;; -----------------------------------------------------------------------------
;;; Dired setup
;;; -----------------------------------------------------------------------------

(setq dired-listing-switches "-agho --group-directories-first")
(put 'dired-find-alternate-file 'disabled nil)

;; Helper packages for a better dired experience
(use-package dired-single)
(use-package dired-collapse)

;; Enable async dired
(autoload 'dired-async-mode "dired-async.el" nil t)
(dired-async-mode 1)

;;; -----------------------------------------------------------------------------
;;; Emacs theme and styling
;;; -----------------------------------------------------------------------------

;; Set the font
;; (set-frame-font "Terminus:pixelsize-18:antialias-none" nil t)
(set-frame-font "Tamsyn10x20:pixelsize-12:antialias-none" nil t)

;; Set the title of the frame to "[current buffer name] - Emacs"
(setq-default frame-title-format '("%b - Emacs"))

;; Just stop bothering me with irrelevant shit
(setq custom-safe-themes t)

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

;;; ----------------------------------------------------------------------------
;;; Version Control
;;; ----------------------------------------------------------------------------

;; Git management with Magit for the win
(straight-use-package 'magit)

;;; ----------------------------------------------------------------------------
;;; Spell checking
;;; ----------------------------------------------------------------------------

(setq ispell-program-name "aspell")
(require 'ispell)

;;; ----------------------------------------------------------------------------
;;; Project/compilation config
;;; ----------------------------------------------------------------------------

;; Improve finding the root of projects
(setq project-vc-extra-root-markers
    '("build.lua" "build.bat" "build.sh" "CMakeLists.txt" "Makefile" ".git"))

(setq compilation-always-kill    t                  ; kill compilation process before starting another
      compilation-ask-about-save nil                ; save all buffers on `compile'
      compilation-scroll-output 'first-error
      compile-command           "luajit build.lua") ; Default compile command.

;; Allow ansi colors in the compilation mode and such.
(require 'ansi-color)

(defun mug--colourful-compilation ()
  "Handle ansi escape sequences from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook #'mug--colourful-compilation)

;;; ----------------------------------------------------------------------------
;;; Mode-mapping by filetype
;;; ----------------------------------------------------------------------------

;; Lightweight C-mode
(require 'simpc-mode)

;; Correctly map file-types to their corresponding modes
(setq auto-mode-alist
      (append
       '(("\\.h\\(h\\|pp\\)?\\'"                                . simpc-mode)
         ("\\.c\\(c\\|pp\\)?\\'"                                . simpc-mode)
         ("\\.\\(?:comp\\|vert\\|geom\\|frag\\|tesc\\|tese\\)$" . simpc-mode)
         ("\\.\\(?:txt\\|md\\)$"                                . indented-text-mode))
       auto-mode-alist))

;;; ----------------------------------------------------------------------------
;;; Improving utilities
;;; ----------------------------------------------------------------------------

;; Use ripgrep whenever available
(defconst mug--ripgrep-available (eval-when-compile (executable-find "rg"))
  "Whether the program Ripgrep is available or not.")
(if mug--ripgrep-available
    (straight-use-package 'rg)
    (setq xref-search-program 'ripgrep))

;; Mini-buffer completion
(use-package vertico
  :commands (vertico-mode)
  :init     (vertico-mode)
  :bind (:map vertico-map
              ("C-j" . vertico-previous)
              ("C-n" . vertico-next))
  :custom   (vertico-cycle t))

;;; ----------------------------------------------------------------------------
;;; Moving through the codebase via tags
;;; ----------------------------------------------------------------------------

(use-package xref
  :custom
  ;; Show results in the minibuffer
  (xref-show-definitions-function #'xref-show-definitions-completing-read)
  (xref-show-xrefs-function       #'xref-show-definitions-completing-read))

(use-package citre
 :defer t
 :bind (:map citre-mode-map
             ("C-c u" . citre-update-this-tags-file))
 :init (require 'citre-config)
 :custom
 (citre-ctags-program                     "ctags")
 (citre-tags-global-cache-dir             (concat mug-emacs-cache-dir "/tags"))
 (citre-default-create-tags-file-location 'in-dir)
 (citre-auto-enable-citre-mode-modes      '(prog-mode))
 :config
 ;; Citre setup
 (setq-default citre-enable-capf-integration nil))
(add-hook 'find-file-hook #'citre-auto-enable-citre-mode)

;;; ----------------------------------------------------------------------------
;;; In-buffer completion and snippets
;;; ----------------------------------------------------------------------------

(use-package corfu
  :custom
  (corfu-cycle                   t) ; Enable cycling for `corfu-next/previous'
  (corfu-auto                    t) ; Enable auto completion
  (corfu-auto-prefix             2) ; Wait for at least 2 characters to be pressed before showing completions
  (corfu-auto-delay            0.1) ; Wait a tiny bit before showing results
  (corfu-count                   5)
  (corfu-preselect         'prompt)
  (corfu-preview-current   'insert)
  (corfu-on-exact-match        nil)
  (tab-always-indent     'complete)
  (corfu-popupinfo-delay '(0.5 . 1.0))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; @TODO: give TempEL a try: https://github.com/minad/tempel
;;(use-package yasnippet
;;  :custom
;;  (yas-snippet-dirs (concat mug-emacs-dir "/snippets")))

;;; -----------------------------------------------------------------------------
;;; C/C++ utility functions
;;; -----------------------------------------------------------------------------

(defun mug-c-msvc-toolchain ()
  "Set MSVC toolchain environment variables.

This is extremely non-portable so my tip for working with this shitty
Windows environment is: open a decent shell, run `vcvarsall.bat x64`
there and copy the results coming from `echo %[INCLUDE | LIB | LIBPATH | PATH]%`
that are relevant for your installation. "
  (interactive)
  (message "Setting MSVC x64 environment variables.")
  (setenv "INCLUDE"
          (concat
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.42.34433/include"
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Auxiliary/VS/include"
           ";" "C:/Program Files (x86)/Windows Kits/10/include/10.0.22621.0/ucrt"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/um"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/shared"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/winrt"
           ";" "C:/Program Files (x86)/Windows Kits/10//include/10.0.22621.0/cppwinrt"
           ";" "C:/Program Files (x86)/Windows Kits/NETFXSDK/4.8/include/um"))

  (setenv "LIB"
          (concat
           "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.42.34433/lib/x64"
           ";" "C:/Program Files (x86)/Windows Kits/NETFXSDK/4.8/lib/um/x64"
           ";" "C:/Program Files (x86)/Windows Kits/10/lib/10.0.22621.0/ucrt/x64"
           ";" "C:/Program Files (x86)/Windows Kits/10//lib/10.0.22621.0//um/x64"))

  (setenv  "LIBPATH"
           (concat
            "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.42.34433/lib/x64"
            ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.42.34433/lib/x86/store/references"
            ";" "C:/Program Files (x86)/Windows Kits/10/UnionMetadata/10.0.22621.0"
            ";" "C:/Program Files (x86)/Windows Kits/10/References/10.0.22621.0"
            ";" "C:/Windows/Microsoft.NET/Framework64/v4.0.30319"))

  (setenv "PATH"
          (concat
           mug-path-env
           ";" "C:/Program Files/Microsoft Visual Studio/2022/Community/VC/Tools/MSVC/14.42.34433/bin/HostX64/x64"
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

;; Automatically set the msvc toolchain bullshit after initialising
(if mug-sys-is-win
    (add-hook 'after-init-hook #'mug-c-msvc-toolchain))

;; TODO: support project include/ directories, not only same-directory structures
(defun mug--c-find-impl-or-decl-file-name ()
  "Find name of the implementation or header of the current buffer."
  (let ((base-file-name (file-name-sans-extension buffer-file-name)))
    (cond
     ;; In case of an implementation file, we search for the header
     ((string-match "\\.c" buffer-file-name)   (concat base-file-name ".h"))
     ((string-match "\\.cpp" buffer-file-name) (concat base-file-name ".hpp"))
     ;; In case of a header, we search for the implementation
     ((string-match "\\.h" buffer-file-name)
      (cond
       ((file-exists-p (concat base-file-name ".c"))   (concat base-file-name ".c"))
       ((file-exists-p (concat base-file-name ".cpp")) (concat base-file-name ".cpp")))))))

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

;;; ----------------------------------------------------------------------------
;;; Automatic buffer editing
;;; ----------------------------------------------------------------------------

;; Trim the extraneous whitespaces before saving files.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(defconst mug-c-formatter "clang-format"
  "The name of the C/C++ formatter executable")
(defconst mug-c-formatter-found (eval-when-compile (executable-find mug-c-formatter))
  "Whether the C/C++ formatter `mug-c-formatter' executable was found or not.")

(defun mug-c-format-buffer ()
  "Format the current buffer."
  (interactive)
  (let ((current-point        (point))
        (current-window-start (window-start)))
    (shell-command-on-region (point-min) (point-max) mug-c-formatter nil t)
    ;; Return the user to the previous state
    (set-window-start (selected-window) current-window-start)
    (goto-char current-point)))

;;; ----------------------------------------------------------------------------
;;; Getting EVIL
;;; ----------------------------------------------------------------------------

(use-package evil
  :defines  (evil-mode evil-emacs-state-modes)
  :init
  (setq-default evil-want-keybinding nil)
  (setq evil-want-integration         t
	    evil-want-C-u-scroll          nil
	    evil-want-C-i-jump            nil
	    evil-respect-visual-line-mode t
	    evil-undo-system              'undo-redo
	    evil-shift-width              4
	    evil-insert-state-cursor      'box
	    evil-normal-state-cursor      'box
	    evil-esc-delay                0)
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

  ;; Disable useless messages...
  (dolist (fn '(evil-forward-char evil-backward-char evil-next-line evil-previous-line evil-line-move))
    (advice-add fn :around #'mug--suppress-message))

  ;; Keybinding goodies
  (use-package evil-collection
    :requires evil
    :defines  (evil-collection-mode-list)
    :commands (evil-collection-init)
    :init
    (setq evil-collection-mode-list (remove 'lispy evil-collection-mode-list))
    (evil-collection-init)
    :custom
    (evil-collection-outline-bind-tab-p nil)
    (evil-collection-want-unimpaired-p  nil))

  ;; Commenting made easier
  (use-package evil-nerd-commenter
    :requires evil))

;;; ----------------------------------------------------------------------------
;;; Keybindings
;;; ----------------------------------------------------------------------------

;; <Escape> from anything
(keymap-global-set "<escape>" 'keyboard-escape-quit)

;; Translate <C-k> into an escape key press
(define-key key-translation-map (kbd "C-k") (kbd "<escape>"))

;; Remove annoying keybindings
(dolist (key '("C-SPC" "C-x C-b" "C-x C-c" "C-x C-d" "C-x C-z" "C-x C-s" "C-x s" "C-x f" "C-h h"
               "C-h k" "C-h j" "C-h l" "C-f" "C-z" "C-j" "M-j"))
    (keymap-global-unset key))

;; This is needed for me because my keyboard uses the Caps Lock as a "magic" key
;; when pressed together with some other key, in order to prevent such a fucking
;; bullshit, I translated every possible shit that could happen into the right
;; thing that it should be bound in the first place.
(define-key key-translation-map (kbd "C-<up>")    (kbd "C-w"))
(define-key key-translation-map (kbd "C-<left>")  (kbd "C-a"))
(define-key key-translation-map (kbd "C-<down>")  (kbd "C-s"))
(define-key key-translation-map (kbd "C-<right>") (kbd "C-d"))

;; Kind-of global motion with j and k
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; We'll be using general for a easier time setting up keybindings.
(require 'general)

;; Keybindings that work for ALL modes when in normal state.
;;
;; @NOTE: I couldn't get this working without general. That's the actual only reason why we
;;        use it in the first place.
(general-override-mode)
(general-def 'normal 'override
  "SPC e"   '(lambda () (interactive) (dired (file-name-directory buffer-file-name)))
  "SPC w"   'save-buffer
  "SPC q"   'kill-buffer-and-window
  "SPC s"   'split-window-right
  "SPC h"   'split-window-below
  "SPC o"   'other-window
  "SPC b b" 'switch-to-buffer
  "SPC f e" 'find-file
  "SPC f o" 'find-file-other-window
  "SPC f f" 'project-find-file
  "SPC f c" 'mug-c-find-corresponding
  "SPC c c" 'project-compile
  "SPC r r" 'project-recompile
  "SPC f z" 'rg
  "SPC f b" 'mug-c-format-buffer)

;; Normal mode keymapping
(general-def 'normal
  "Y"   '(lambda () (interactive) (evil-yank (point) (line-end-position)))
  "g d" 'citre-jump
  "g b" 'citre-jump-back
  "g o" 'xref-find-definitions-other-window
  "M-;" 'evilnc-comment-or-uncomment-lines)

;; Dired specific keymapping
(general-define-key
 :states  'normal
 :keymaps 'dired-mode-map
 "h" '(lambda () (interactive) (find-alternate-file ".."))
 "H" 'dired-omit-mode
 "l" 'dired-single-buffer
 "y" 'dired-ranger-copy
 "X" 'dired-ranger-move
 "p" 'dired-ranger-paste)

;; Elisp specific keymapping
(general-define-key
 :states  'normal
 :keymaps 'emacs-lisp-mode-map
 "C-c e" 'eval-buffer)

;;; -----------------------------------------------------------------------------

(provide 'init)

;;; init.el ends here
