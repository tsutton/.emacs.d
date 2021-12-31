;;; init-generic-text.el --- Load my emacs config  -*- lexical-binding: t; -*-
;;; Commentary:

;;; Code:

;; undo-tree replaces emacs' builtin undo-management system with a tree instead.
;; with built-in undo, when you undo one or more times, and then type, the undo
;; system creates a single flat list of changes
;; e.g. type A, then B, then C, then undo twice, then type D, your buffer be AD and
;; the undo list is this:
;; A, B, C, (undo C), (undo B), D
;; Thus if you want to undo back again, you need to undo your undos.
;; undo-tree replaces this system with one that matches my intuition more conceptually
;; if I undo back to just A, and then type D, it keeps a TREE of edits, like so
;;      A
;;     / \
;;    B   D
;;    |
;;    C
;; and doesn't track undos as operations that can, themselves, be undone.
;; Plus it provides a nice interface to open up this tree in a buffer and navigate
;; up and down the history. Even when the tree is actually flat, this can be handy
;; for finding the correct stopping point when undoing a lot of stuff.
(use-package undo-tree
  :ensure t
  :diminish
  :config
  (global-undo-tree-mode)
  )

;; browse-kill-ring makes it so that pressing M-y when the previous command
;; was NOT a yank pulls up the kill ring in a buffer for navigation
;; It's an alternate kill-ring browser to e.g. counsel-yank-pop, which I don't like as much
;; since kill-region items are too big really to navigate in mini-buffer.
(use-package browse-kill-ring
  :ensure t
  :config
  (browse-kill-ring-default-keybindings)
  )

;; Whitespace mode
;; This causes trailing spaces and lines that only contain whitespace to be highlighted,
;; but also causes those spaces to be removed on save in both text-mode and prog-mode.
;; might switch the after save hook to https://github.com/purcell/whitespace-cleanup-mode
(require 'whitespace)
(setq whitespace-style '(face trailing empty))
(global-whitespace-mode t)
(diminish 'global-whitespace-mode)
(defun enable-on-save-whitespace ()
  "Add a hook to \"before-save-hook\" to cleanup trailing whitespace."
  (add-hook 'before-save-hook 'whitespace-cleanup 0 t)
  )
(add-hook 'text-mode-hook 'enable-on-save-whitespace)
(add-hook 'prog-mode-hook 'enable-on-save-whitespace)

;; save-place-mode saves the location of point in each file you visit,
;; and restores point to that place when revisiting it.
(save-place-mode 1)

;; zop-to-char is a more powerful zap-to-char.
;; After typing a character, it marks the region that would be killed.
;; Then, you can either confirm and kill it, or navigate to next/previous instances
;; of that character before doing so. You can also copy instead of killing.
;; This handles the common case where I want to kill to, say, ), but didn't notice
;; that it's actually the second ) that I want to zap to, there was an earlier one as well.
;; Also, I find that I want to zap-up-to more often then zap-to, so let's bind M-z to that one
;; while still having zap-to on M-Z.
(use-package zop-to-char
  :ensure t
  :commands (zop-to-char zop-up-to-char)
  :bind
  (( "M-z" . 'zop-up-to-char)
   ( "M-Z" . 'zop-to-char))
  )

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; set-goal-column is mostly useful with keyboard macros
(put 'set-goal-column 'disabled nil)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq-default fill-column 100)

(setq delete-active-region nil)
(setq require-final-newline t)

(provide 'init-generic-text)
;;; init-generic-text.el ends here
