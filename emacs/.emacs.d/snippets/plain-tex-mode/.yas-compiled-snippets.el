;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
		     '(("tt" "\\\\text{$1}$0" "Text inside math" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/tt" nil nil)
		       ("->" "\\\\to" "To arrow" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/to" nil nil)
		       ("thm" "\\\\begin{theorem}\\label{thm: $1}\n  $0\n\\\\end{theorem}" "theorem" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/thm" nil nil)
		       ("sq" "\\\\sqrt{$1}$0" "square root" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/sq" nil nil)
		       ("set" "\\\\{$1\\\\}$0" "Set" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/set" nil nil)
		       ("prop" "\\\\begin{proposition}\\label{prop: $1}\n  $0\n\\\\end{proposition}" "Proposition" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/prop" nil nil)
		       ("proof" "\\\\begin{proof}\n  $0\n\\\\end{proof}" "Proof" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/proof" nil nil)
		       ("!=" "\\\\neq" "neq" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/neq" nil nil)
		       ("\\\\\\" "\\\\setminus" "setminus" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/minus" nil nil)
		       ("m" "\\\\($0\\\\)" "Inline math" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/m" nil nil)
		       ("lr" "\\\\left$1 $2 \\\\right$1$0" "Left Right" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/lr" nil nil)
		       ("<=" "\\\\leq" "leq" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/leq" nil nil)
		       ("lem" "\\\\begin{lemma}\\label{lem: $1}\n  $0\n\\\\end{lemma}" "Lemma" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/lem" nil nil)
		       ("item" "\\\\begin{itemize}\n  \\\\setlength\\\\itemsep{0.0em}\n  \\\\item $0\n\\\\end{itemize}" "Itemize" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/item" nil nil)
		       ("inv" "^{-1}$0" "Inverse" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/inv" nil nil)
		       ("=>" "\\\\implies" "Implies" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/implies" nil nil)
		       (">=" "\\\\geq" "geq" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/geq" nil nil)
		       ("//" "\\\\frac{$1}{$2}$0" "Fraction" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/frac" nil nil)
		       ("floor" "\\\\left\\\\lfloor $1 \\\\right\\\\rfloor$0\n" "Floor" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/floor" nil nil)
		       ("eq" "\\\\begin{equation}\\\\label{eq: $1}\n  $0\n\\\\end{equation}" "Equation" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/eq" nil nil)
		       ("enum" "\\\\begin{enumerate}\n  \\\\setlength\\\\itemsep{0.0em}\n  \\\\item $0\n\\\\end{enumerate}" "Enumerate" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/enum" nil nil)
		       ("dm" "\\\\[\n  $0\n\\\\]" "Display math" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/dm" nil nil)
		       ("diag" "\\\\begin{tikzcd}\n  $0\n\\\\end{tikzcd}" "tikzcd diagram" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/diag" nil nil)
		       ("def" "\\\\begin{definition}\\label{def: $1}\n  $0\n\\\\end{definition}" "Definition" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/def" nil nil)
		       ("cor" "\\\\begin{corollary}\\label{cor: $1}\n  $0\n\\\\end{corollary}" "Corollary" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/cor" nil nil)
		       ("ceil" "\\\\left\\\\lceil $1 \\\\right\\\\rceil$0" "Ceil" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/ceil" nil nil)
		       ("cases" "\\\\begin{cases}\n  $0\n\\\\end{cases}" "Cases" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/cases" nil nil)
		       ("bmat" "\\\\begin{bmatrix}\n  $0\n\\\\end{bmatrix}" "Braces matrix" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/bmat" nil nil)
		       ("begin" "\\\\begin{$1}\n  $0\n\\\\end{$1}" "begin env" nil nil nil "/home/luiz/.emacs.d/snippets/latex-mode/begin" nil nil)
		       ("ali" "\\\\begin{align*}\n  $0\n\\\\end{align*}" "Align" nil
			("Math")
			nil "/home/luiz/.emacs.d/snippets/latex-mode/ali" nil nil)))


;;; Do not edit! File generated at Fri Apr 22 23:00:42 2022
