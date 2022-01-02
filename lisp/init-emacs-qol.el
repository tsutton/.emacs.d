;;; init-emacs-qol.el --- TODO  -*- lexical-binding: t; -*-
;;; Commentary:

;; buffer management, history of commands, etc

;;; Code:

;; recentf tracks recently visited files and integrates into stuff.
(setq-default
 recentf-max-saved-items 300 ; default is 20
 recentf-max-menu-items 10 ; default is 10
 )

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind ([remap other-window] . ace-window)
  ;; Other possibly useful variables:
  ;; - aw-scope (by default it's 'global, but might want 'frame instead)
  ;; - the faces used by ace
  )

;; savehist keeps track of minibuffer history
(require 'savehist)
(setq savehist-additional-variables
;;       ;; search entries
      '(search-ring regexp-search-ring))
;;       ;; save every minute
;;       savehist-autosave-interval 60
(savehist-mode +1)

(setq confirm-kill-emacs 'y-or-n-p)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")

(fset 'yes-or-no-p 'y-or-n-p)

;; avy is a package for navigation - it lets you easily move point to somewhere
;; else on screen. I try to use it when I remember, although honestly
;; most of the time I just use isearch for this functionality.
;; avy-goto-char-timer lets you type a few characters, then when you stop typing,
;; for each place visible in a window, it shows a short sequence of chars
;; and if you follow up by typing one of those, it jumps point to that place.
;; e.g. if I see the word edge on screen, and I want to navigate to it,
;; I do C-c j edge <pause>, then it'll show a character for each occurrence
;; of edge in view, and I type one of those to pick.
(use-package avy
  :ensure t
  :commands (avy-goto-char-timer)
  :bind ("C-c j" . avy-goto-char-timer)
  :config
  (setq avy-background t)
  )

;; which-key makes it so that when you have entered a prefix but not completed
;; the command, you get a preview in the minibuffer of what your options are
;; e.g. after typing C-h, I get a list of all the things I can type next to get help
;; on different topics.
;; It's a nice little one since it just enhances regular flows without needing to learn
;; or do anything differently :)
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode +1)
  )


;; crux provides little improvements to a bunch of thing
;; I only use a subset of them, on the theory that for me, if there are too many
;; key combinations/functions to learn, it ends up a detriment if they all just
;; are all relatively marginal
(use-package crux
  :ensure t
  :commands (crux-kill-line-backwards
	     crux-move-beginning-of-line
	     crux-delete-file-and-buffer
	     crux-rename-buffer-and-file)
  :bind
  (
   ;; kills the line except for the leading indentation
   ("M-<backspace>" . 'crux-kill-line-backwards)
   ;; moves to the start of the text (after whitespace) at the beginning of line
   ;; or, if already in front of all text, toggles between first column and first text
   ([remap move-beginning-of-line] . 'crux-move-beginning-of-line)
   ("C-c D" . 'crux-delete-file-and-buffer)
   ;; The only awkward thing about this one is that it doesn't integrate with ivy
   ;; But usually renaming is simple enough that it's still worth using.
   ("C-c r" . 'crux-rename-buffer-and-file)
   ;; there's more than can be found in prelude, if curious
   ))

(provide 'init-emacs-qol)
;;; init-emacs-qol.el ends here
