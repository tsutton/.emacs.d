;;; init-go.el --- Configuration for editting Go code -*- lexical-binding: t; -*-
;;; Commentary:
;;

;;; Code:

(use-package go-mode
  :ensure t
  :defer t
  :config
  (setq gofmt-command "goimports") ; I don't think this is actually used by LSP though
  (add-hook 'go-mode-hook 'lsp-deferred)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'go-eldoc-setup)
  (lsp-register-custom-settings
   '(("gopls.staticcheck" t t)))
  )

(provide 'init-go)
;;; init-go.el ends here
