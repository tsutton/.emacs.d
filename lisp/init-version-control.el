;;; init-look-and-feel.el --- Load my emacs config  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Some research seems to indicate that there's an issue with rustic and project.
;; Even though project is built-in, without explicitly using this (which implicitly uses
;; straight-use-package), magit C-n and C-p don't work for whatever reason.
;; See https://old.reddit.com/r/emacs/comments/po9cfj/magit_commands_broken/
(use-package project)

(use-package magit
  :defer t
  :ensure t
  :config
  (diminish 'auto-revert-mode)
  )

;; forge lets you interact with things like github PRs and issues from inside magit
;; It requires external setup for creating and storing, API tokens plus setting
;; variables (e.g. github username)
;; For more info, see https://magit.vc/manual/forge/index.html#Top
(use-package forge
  :ensure t
  :after magit)

;; ==from prelude== (modified to use use-package)
;; diff-hl shows when a part of a version-controlled file has been modified by changing
;; the color of the window's fringe. It also provides commands to see the difference and
;; to jump between these modified hunks, but I haven't integrated those into my flow.
(use-package diff-hl
  :ensure t
  :diminish
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )

(provide 'init-version-control)
;;; init-version-control.el ends here
