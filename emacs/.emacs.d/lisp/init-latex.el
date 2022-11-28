;;; init-latex.el --- latex config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (setq ispell-program-name "hunspell")
;; (setq ispell-hunspell-dict-paths-alist
;;   (setq ispell-local-dictionary "en_GB")

;;   ;; (setq ispell-local-dictionary-alist
;;   ;;   ;; Please note the list `("-d" "en_US")` contains ACTUAL parameters passed to hunspell
;;   ;;   '(("en_GB" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_GB") nil utf-8)))

;;   ;; the following line won't make flyspell-mode enabled by default as you might think
;;   (flyspell-mode 1)

;;   ;; ispell-word for showing correcting options of the current misspelled word
;;   ;; (global-set-key (kbd "M-\\") 'ispell-word)
;;   ;;   '(("en_GB" "/usr/share/myspell/dicts/en_GB.aff")))
;;   )

;; (flyspell-check-tex-math-command t)

;;; AuCTeX configuration

;; Currently the support for autocompilation is a mess:
;; open a terminal and run `latexmk -pvc` on the project of interest
(use-package latex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :straight auctex
  :hook ((LaTeX-mode . yas-minor-mode)
         ;; Make AUCTeX aware of multifile doc structure
         (LaTeX-mode . TeX-source-correlate-mode)
         (LaTeX-mode . flyspell-mode))
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
  (setq LaTeX-item-indent -2)

  ;; Compilation and PDF
  (setq TeX-PDF-mode t
        TeX-source-correlate-mode t
        TeX-source-correlate-start-server t)

  ;; References setup
  (setq-default reftex-plug-into-AUCTeX t)

  ;; Fold by section using C-tab
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (define-key LaTeX-mode-map (kbd "<C-tab>") 'outline-toggle-children)
  (define-key LaTeX-mode-map (kbd "C-g C-q") 'LaTeX-fill-paragraph)
  (define-key LaTeX-mode-map (kbd "C-f C-r") 'reftex-cleveref-cref))

;;; Visual editing

;; Visualization with buffer preview --- use C-c C-p
(add-hook 'LaTeX-mode-hook
          (defun preview-larger-previews ()
            (setq preview-scale-function
                  (lambda () (* 1.5 ;; Change this scale if need be
                           (funcall (preview-scale-from-face)))))))


;;; Latex text input configuration:

;; cdlatex stuff
(use-package cdlatex
  :straight t
  :hook ((LaTeX-mode . turn-on-cdlatex)
	 (cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :bind (:map cdlatex-mode-map
              ("<tab>" . cdlatex-tab))
  :config
  (setq cdlatex-use-dollar-to-ensure-math 0) ;; disables the use of dollar signs
  (local-unset-key (kbd "^"))
  (setq cdlatex-paired-parens "{([")
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

;; Auto-expanding snippets
(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'latex-mode
    ;; Math environments
    "mk" (lambda () (interactive)
           "Inline math"
           (yas-expand-snippet "\\\\($0\\\\)"))
    "mmk" (lambda () (interactive)
           "Display math"
           (yas-expand-snippet "\\[\n$0\n\\]"))
    "!ali" (lambda () (interactive)
             "align environment"
             (yas-expand-snippet "\\begin{align*}\n$0\n\\end{align*}"))
    "!gather" (lambda () (interactive)
                "gather environment"
                (yas-expand-snippet "\\begin{gather*}\n$0\n\\end{gather*}"))
    "!eq" (lambda () (interactive)
            "equation environment"
            (yas-expand-snippet "\\begin{equation}\\label{eq:$1}\n$0\n\\end{equation}"))

    ;; Text environments
    "!p" (lambda () (interactive)
               "proposition environment"
               (yas-expand-snippet
                "\\begin{proposition}\n\\label{prop:$1}\n$0\n\\end{proposition}"))
    "!t" (lambda () (interactive)
               "theorem environment"
               (yas-expand-snippet
                "\\begin{theorem}\n\\label{thm:$1}\n$0\n\\end{theorem}"))
    "!!p" (lambda () (interactive)
               "proof environment"
               (yas-expand-snippet
                "\\begin{proof}\n$0\n\\end{proof}"))
    "!l" (lambda () (interactive)
               "lemma environment"
               (yas-expand-snippet
                "\\begin{lemma}\n\\label{lem:$1}\n$0\n\\end{lemma}"))
    "!c" (lambda () (interactive)
               "corollary environment"
               (yas-expand-snippet
                "\\begin{corollary}\n\\label{cor:$1}\n$0\n\\end{corollary}"))
    "!d" (lambda () (interactive)
                "definition environment"
                (yas-expand-snippet
                 "\\begin{definition}\n\\label{def:$1}\n$0\n\\end{definition}"))
    "!exp" (lambda () (interactive)
                "example environment"
                (yas-expand-snippet
                 "\\begin{example}\n\\label{exp:$1}\n$0\n\\end{example}"))
    "!r" (lambda () (interactive)
                "remark environment"
                (yas-expand-snippet
                 "\\begin{remark}\n\\label{rem:$1}\n$0\n\\end{remark}"))
    "!not" (lambda () (interactive)
                "notation environment"
                (yas-expand-snippet
                 "\\begin{notation}\n\\label{not:$1}\n$0\n\\end{notation}"))
    "!enum" (lambda () (interactive)
                "enumerate environment"
                (yas-expand-snippet
                 "\\begin{enumerate}[(a)]\\setlength\\itemsep{0em}\n\\item$0\n\\end{enumerate}"))
    "!item" (lambda () (interactive)
                "itemize environment"
                (yas-expand-snippet
                 "\\begin{itemize}\\setlength\\itemsep{0em}\n\\item$0\n\\end{itemize}"))

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
      "==" "\\iso"
      "=>" "\\nat"

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
               (yas-expand-snippet "\\begin{cases}\n  $0\n\\end{cases}"))))

(provide 'init-latex)
;;; init-latex.el ends here
