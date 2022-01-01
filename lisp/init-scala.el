;;; init-scala.el --- Configuration for editting Scala -*- lexical-binding: t; -*-
;; Enable scala-mode for highlighting, indentation and motion commands

;;; Commentary:
;;

;;; Code:

(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :defer t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)
(use-package lsp-metals
  :ensure t
  :defer t
  :config (setq lsp-metals-treeview-show-when-views-received t))

(provide 'init-scala)
;;; init-scala.el ends here
