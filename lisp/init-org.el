;;; init-org.el --- Setup org mode -*- lexical-binding: t; -*-
;;; Commentary:
;;

;;; Code:

(use-package org
  :ensure nil
  :defer t
  :config
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (setq
   org-agenda-custom-commands
   '(("x" "Un-scheduled" tags "+TODO=\"TODO\"-SCHEDULED<>\"\"-DEADLINE<>\"\"" ((org-agenda-sorting-strategy '(priority-down))))
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
   ((equal (downcase (system-name)) "visionary-windo")
    (setq org-agenda-files (quote ("c:/Users/taylo/OneDrive/todo.org" "c:/Users/taylo/OneDrive/notes.org" "c:/Users/taylo/OneDrive/cs.org")))
    (setq org-directory "c:/Users/taylo/OneDrive")
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
  :ensure nil
  :bind ("C-c a" . org-agenda)
  )

(use-package org-roam
  :ensure t
  :config
  (setq org-roam-directory (concat org-directory "/" "org-roam"))
  (org-roam-db-autosync-mode)
  ;; setup simplified from  https://jethrokuan.github.io/org-roam-guide/
  (setq org-roam-capture-templates
	'(("m" "main" plain
           "%?"
           :if-new (file+head "main/%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "reference/%<%Y%m%d%H%M%S>-${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  ;; TODO
  ;; (cl-defmethod org-roam-node-type ((node org-roam-node))
  ;;   "Return the TYPE of NODE."
  ;;   (condition-case nil
  ;;       (file-name-nondirectory
  ;;        (directory-file-name
  ;;         (file-name-directory
  ;;          (file-relative-name (org-roam-node-file node) org-roam-directory))))
  ;;     (error "")))
  ;; (setq org-roam-node-display-template
  ;;   (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  ;; alternately, there's org-roam-node-annotatoin-function custom var

  (defun ts/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'ts/tag-new-node-as-draft)

  :bind
  (("C-c n f" . org-roam-node-find)
   :map org-mode-map
   ("C-c n i" . org-roam-node-insert)
   ("C-c n b" . org-roam-buffer-toggle)
   )
  )

(provide 'init-org)
;;; init-org.el ends here
