;;; init-vterm.el --- vterm config  --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :straight t)

(defun vterm-directory-sync ()
  "Synchronize current working directory."
  (interactive)
  (when vterm--process
    (let* ((pid (process-id vterm--process))
           (dir (file-truename (format "/proc/%d/cwd/" pid))))
      (setq default-directory dir))))

(provide 'init-vterm)
;;; init-vterm.el ends here
