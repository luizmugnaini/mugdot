;;; init-completion.el --- completion config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)
  :custom
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Automatic bracket pairing
(electric-pair-mode 1)

;; Text completion framework
(use-package company
  :straight t
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
  :straight t
  :hook ((LaTeX-hook . yas-minor-mode))
  :config
  (setq yas-snippet-dirs '("~/.mugdot/emacs/.emacs.d/snippets/")))

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
  :hook ((org-mode . aas-activate-for-major-mode)
         (text-mod . aas-activate-for-major-mode)
         (markdown-mode . aas-activate-for-major-mode))
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
    ";U" "Ú"))

(provide 'init-completion)
;;; init-completion.el ends here
