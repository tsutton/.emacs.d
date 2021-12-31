;;; init-lsp.el --- TODO -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; Code:

(defun enable-on-save-lsp-format ()
  "Add a hook to \"before-save-hook\" to cleanup trailing whitespace."
  (add-hook 'before-save-hook 'lsp-format-buffer 0 t)
  )

(use-package lsp-mode
  :defer t
  :ensure t
  :commands (lsp
	     lsp-deferred
	     lsp-format-buffer
	     lsp-organize-imports
	     lsp-register-custom-settings
	     )
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . enable-on-save-lsp-format)
         (lsp-mode . lsp-lens-mode)
	 (scala-mode . lsp)
	 )
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      )
  )

(use-package lsp-ivy
  :ensure t
  :defer t
  :commands lsp-ivy-workspace-symbol
  )

;; Use the Debug Adapter Protocol for running tests and debugging
(use-package posframe
  ;; Posframe is a pop-up tool that must be manually installed for dap-mode
  :ensure t
  )

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode)
  :config
  (require 'dap-go) ;; TODO move this to a more appropriate place?
  (dap-go-setup)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
