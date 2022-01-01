;;; init-completion.el --- TODO -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches) ;; defaults to Shift-SPC
  :diminish
  )

(use-package swiper
  :ensure t
  :commands (swiper-isearch swiper-isearch-backward)
  :config
  (global-set-key [remap isearch-forward] 'swiper-isearch)
  (global-set-key [remap isearch-backward] 'swiper-isearch-backward)
  (setq swiper-goto-start-of-match t)
  )

;; Just enabling ivy already sets ivy as the completion function for e.g. find-file
;; According to the docs, the counsel versions of these functions don't just use ivy,
;; they have extra features built in as well.
;; Namely, they provide extra actions that can be taken, for example, when in
;; counsel-find-file, M-o j will open the file in the other window (or it can be
;; inserted, etc...)
;; TODO I thought counsel (or maybe in combination with savehist) would order M-x
;;      results by recent, but it doesn't seem to do that.
(use-package counsel
  :ensure t
  :diminish
  ;; TODO defer loading and use :commands with these binds
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "<f2> j") 'counsel-set-variable)
  (global-set-key (kbd "C-x b") 'ivy-switch-buffer)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  ;; There are other interesting options that could be enabled:
  ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-rg)
  )

;; prescient is a system for reordering the candidates for autocomplete systems.
;; I use it with ivy and company (configured below).
;; Ivy and company still do their thing as far as interface and providing the list of
;; possible doodads, then prescient reorders them.
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode)
  (setq prescient-sort-full-matches-first t)
  )

(use-package ivy-prescient
  :ensure t
  ;; according to docs, must be loaded after counsel, since both modify ivy
  :after counsel
  :diminish
  :config
  (ivy-prescient-mode)
  )

(use-package company-prescient
  :ensure t
  :diminish
  :config
  (company-prescient-mode)
  )

;; Similar to how counsel enhances builtins, counsel-projectile takes things
;; a step further than the basic usage of ivy as completion for projectile.
;; Note, there's a funky bug with counsel-projectile-switch-project where pressing
;; ivy-restrict-to-input doesn't actually restrict (though it does erase input).
;; TODO either fix that bug, or rebind the non-counsel version directly.
(use-package counsel-projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode 1)
  )

(provide 'init-completion)
;;; init-completion.el ends here
