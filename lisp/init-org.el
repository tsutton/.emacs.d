;;; init-org.el --- Setup org mode -*- lexical-binding: t; -*-
;;; Commentary:
;;

;;; Code:

(use-package org
  :straight nil
  :defer t
  :config
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (setq
   org-agenda-custom-commands
   '(("x" "Un-scheduled" tags "+TODO=\"TODO\"-SCHEDULED<>\"\"-DEADLINE<>\"\"" nil)
     ("n" "Agenda and all TODOs"
      ((agenda #1="")
       (alltodo #1#))))
   org-adapt-indentation nil
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-todo-list-sublevels nil
   org-hide-leading-stars t
   org-hierarchical-todo-statistics nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets '((org-agenda-files :maxlevel . 2))
   )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (calc . t))
   )
  (cond
   ((equal (system-name) "quillen.local")
    (setq org-agenda-files (quote ("~/general.org")))
    (setq org-directory "~/")
    )
   (t
    (setq org-agenda-files (quote ("~/OneDrive/todo.org" "~/OneDrive/notes.org" "~/OneDrive/cs.org")))
    (setq org-directory "~/OneDrive")
    )
   )
  :bind (:map org-mode-map
	      ("C-M-j" . org-shiftmetaleft)
	      ("C-M-k" . org-shiftmetaright)
	      )
  )

(use-package org-agenda
  :straight nil
  :bind ("C-c a" . org-agenda)
  )

(provide 'init-org)
;;; init-org.el ends here
