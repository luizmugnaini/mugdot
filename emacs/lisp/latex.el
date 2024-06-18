;;; ============================================================================
;;; Latex configuration:
;;; ============================================================================

(use-package latex
  :defer t
  :mode ("\\.tex\\'" . LaTeX-mode)
  :straight auctex
  :hook ((LaTeX-mode . yas-minor-mode)
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . flyspell-mode)
	 (LaTeX-mode . outline-minor-mode))
  :bind (:map LaTeX-mode-map
	      ("<C-tab>" . outline-toggle-children)
	      ("C-c C-c" . tex-compile)
	      ("C-g C-q" . LaTeX-fill-paragraph)
	      ("C-f C-r" . reftex-cleveref-cref))
  :config
  (when mug-sys-is-win
    (setq exec-path (append exec-path '(concat mug-home-dir "/AppData/Local/Programs/MiKTeX/miktex/bin/x64"))))

  ;; Basic settings
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq visual-fill-column-center-text t)

  ;; Indentation settings
  (setq LaTeX-indent-level 0
	LaTeX-item-indent -2)
        TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  ;; References setup
  (setq-default reftex-plug-into-AUCTeX t)

  (use-package cdlatex
    :after yasnippet
    :hook ((LaTeX-mode  . turn-on-cdlatex)
	   (cdlatex-tab . yas-expand)
           (cdlatex-tab . mug-cdlatex-in-yas-field))
    :bind (:map cdlatex-mode-map
		("<tab>" . cdlatex-tab))
    :config
    (keymap-local-unset "^")
    (setq cdlatex-use-dollar-to-ensure-math 0 ;; disable the use of dollar signs
	  cdlatex-paired-parens "{(["
	  cdlatex-simplify-sub-super-scripts t)

    ;; YaSnippet integration
    (defun mug-cdlatex-in-yas-field ()
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
      "Jump to the next Yas field correctly with cdlatex active."
      (interactive)
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand))))

    ;; Auto-expanding snippets
    (use-package aas
      :hook (LaTeX-mode . aas-activate-for-major-mode)
      :commands (aas-set-snippets)
      :config
      (aas-set-snippets 'text-mode
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
	"!m" (lambda () (interactive)
               "Display math"
               (yas-expand-snippet "\\[\n$0\n\\]"))
	"!ali" (lambda () (interactive)
		 "align environment"
		 (yas-expand-snippet "\\begin{align*}\n$0\n\\end{align*}"))
	"!g" (lambda () (interactive)
               "gather environment"
               (yas-expand-snippet "\\begin{gather*}\n$0\n\\end{gather*}"))
	"!eq" (lambda () (interactive)
		"equation environment"
		(yas-expand-snippet "\\begin{equation}\\label{eq:$1}\n$0\n\\end{equation}"))

	"!beg" (lambda () (interactive)
                 "begin environment"
                 (yas-expand-snippet
                  "\\begin{$1}\n$0\n\\end{$1}"))
	"!enum" (lambda () (interactive)
                  "enumerate environment"
                  (yas-expand-snippet
                   "\\begin{enumerate}[(a)]\\setlength\\itemsep{0em}\n\\item$0\n\\end{enumerate}"))
	"!item" (lambda () (interactive)
                  "itemize environment"
                  (yas-expand-snippet
                   "\\begin{itemize}\\setlength\\itemsep{0em}\n\\item$0\n\\end{itemize}"))
	:cond #'texmathp
	"->" "\\to"
	"-->" "\\longrightarrow"
	"!>" "\\mapsto"
	"ox" "\\otimes"
	"opp" "\\oplus"
	"OO" "\\infty"
	"**" "\\times"
	"cc" "\\subseteq"
	";sm" "\\setminus"
	"inn" "\\in"
	"inv" "^{-1}"
	"!=" "\\neq"
	":=" "\\coloneq"
	"==" "\\iso"
	"//" (lambda () (interactive)
               "Fraction"
               (yas-expand-snippet "\\frac{$1}{$2}$0"))
	"tt" (lambda () (interactive)
               "Text in math environment"
               (yas-expand-snippet "\\text{$1}$0"))
	";set" (lambda () (interactive)
		 "Collection --- set"
		 (yas-expand-snippet "\\\\{$1\\\\}$0"))
	"diag" (lambda () (interactive)
		 "diagram environment"
		 (yas-expand-snippet "\\begin{tikzcd}\n  $0\n\\end{tikzcd}"))
	"bmat" (lambda () (interactive)
		 "Matrix"
		 (yas-expand-snippet "\\begin{bmatrix}\n  $0\n\\end{bmatrix}"))))
