;;; init-org.el --- org config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun mug/org-mode-setup ()
  "Orgmode setup."
  (org-indent-mode)
  (variable-pitch-mode 0) ;; Fira is horrible with this thing on
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . mug/org-mode-setup)
  :custom
  (org-ellipsis " ▾")
  (org-directory "~/Org")

  ;; ---------------- Agenda stuff ---------------
  (org-deadline-warning-days 14)
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time) ;; Register the time of conclusion
  (org-log-into-drawer t)

  (org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)"
                "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)"
                "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

  ;; Configure custom agenda views
  (org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE"
                 ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

  (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp "~/Org/Agenda/Tasks.org" "Active")
       "* TODO %?\n  %U\n  %a\n" :empty-lines 1)))

  (define-key global-map (kbd "C-c t")
    (lambda () (interactive) (org-capture nil "tt")))

  ;; Refiling: archive concluded tasks and such
  (org-refile-targets
    '(("~/Org/Agenda/Archive.org" :maxlevel . 1)
      ("~/Org/Agenda/Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Tags
  (org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@uni" . ?U)
       ("@home" . ?H)
       ("@work" . ?W)
       ("@project" . ?P)
       ("study" . ?s)
       ("email" . ?e)
       ("appointment" . ?a)
       ("note" . ?n)
       ("homework" . ?h)
       ("idea" . ?i)))

  (org-agenda-files
   '("Agenda/Tasks.org"
     "Agenda/Research.org"
     "Agenda/Projects.org"
     "Agenda/Reading.org")))

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

(provide 'init-org)
;;; init-org.el ends here
