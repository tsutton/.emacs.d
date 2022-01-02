;;; init-programming.el --- TODO -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

;; company is a great autocomplete frontend for editing
;; It integrates well with most backends, including lsp-mode
;; In other words, lsp-mode's connection to the language server
;; provides the possibilities for what the completion is,
;; but it's company-mode that shows them to me and allows me to pick among them
(use-package company
  :ensure t
  :diminish
  :commands (global-company-mode)
  :hook (after-init . global-company-mode)
  :bind (
	 :map company-mode-map
	      ("M-/" . company-complete)
	      ([remap completion-at-point] . company-complete)
	      ([remap indent-for-tab-command] . company-indent-or-complete-common)
	 )
  :config
  (setq company-minimum-prefix-length 1 ;; default is 3
	company-idle-delay 0.0 ;; default is 0.2
	company-show-quick-access t
	company-global-modes '(not org-mode)
	)
)

(use-package projectile
  :defer t
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :config
  ;; (setq projectile-completion-system 'ivy)
  ;; The projectile mode line is too long! I don't need to know the type of
  ;;  project, I likely already know that immediately from the name.
  (defun projectile-custom-mode-line ()
    (let ((project-name (projectile-project-name)))
      (format " proj:%s"
              (or project-name "-"))))
  (setq projectile-mode-line-function 'projectile-custom-mode-line)
  )

(use-package smartparens
  :ensure t
  :diminish
  :hook ((prog-mode . smartparens-mode)
	 (prog-mode . show-smartparens-mode))
  :config
  (require 'smartparens-config) ; sets up default config
  ;; smartparens has a bunch of keybindings for navigating among parens
  ;; and moving things in and out, but they don't seem useful outside of lisp
  ;; so I won't enable them.
  )

(global-set-key (kbd "M-[") 'beginning-of-defun)
(global-set-key (kbd "M-]") 'end-of-defun)

(diminish 'eldoc-mode)

(setq sp-highlight-pair-overlay nil)

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'yaml-mode-hook #'subword-mode) ; yaml-mode doesn't inherit prog-mode

;; ==from prelude==
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(use-package flycheck
  :ensure t
  :diminish
  :config
  (global-flycheck-mode)
  )

(provide 'init-programming)
;;; init-programming.el ends here
