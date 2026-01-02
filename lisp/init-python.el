;;; init-python.el --- Configure completion for user input -*- lexical-binding: t; -*-
;;; Commentary:
;; very much a WIP
;;; Code:

(use-package lsp-pyright
  :ensure t
  :custom (lsp-pyright-langserver-command "pyright") ;; or basedpyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred

(use-package python-black
  :ensure t
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode))

(provide 'init-python)
;;; init-python.el ends here
