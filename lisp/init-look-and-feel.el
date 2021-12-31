;;; init-look-and-feel.el --- Load my emacs config  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; Highlight the line that point is located on! Nifty little mode.
(global-hl-line-mode +1)

;; hl-todo-mode provides highlighting for TODO and NOTE and FIXME (by default, it's configurable)
(use-package hl-todo
  :ensure t
  :diminish
  :config
  (global-hl-todo-mode)
  )

;; rainbow-delimiters makes parens, brackets, etc different colors depending in nesting depth
;; It's a bit subtle so I don't get much value of it, but it's better than nothing.
;; The colors *are* configurable, but eh.
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;; THEMES
;; (use-package ample-theme
;;   :init (progn (load-theme 'ample t)
;;   (load-theme 'ample-flat t)
;;   (load-theme 'ample-light t t)
;;   (enable-theme 'ample)
;; 	       )
;;   :defer t
;;   :ensure t
;;   :config
;;   (load-theme 'ample t)
;;   )

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-molokai t t)
  ;; (load-theme 'doom-Iosvkem t t)
  ;; (load-theme 'doom-tomorrow-night t t)
  ;; (load-theme 'doom-vibrant)
  )
;; It's really close between these four themes and ample above...
;; I think Iosvkem > vibrant > molokai
;; tomorrow-night somewhere with the first two, better than molokai
;; not sure how ample compares, but ample > ample-flat.

;; example theme customization:
;; (with-eval-after-load "ample-theme"
;;  (custom-theme-set-faces
;;    'ample
;;    ;; this will overwride the color of strings just for ample-theme
;;    '(font-lock-string-face ((t (:foreground "#bdba81"))))))

;; =from prelude=
;; (except I removed the "Prelude" part)
(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq scroll-preserve-screen-position t)
(setq blink-cursor-mode nil)
(setq column-number-mode t)

(provide 'init-look-and-feel)
;;; init-look-and-feel.el ends here
