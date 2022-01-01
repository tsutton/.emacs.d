;;; init-term.el --- Setup terminals within emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; vterm is a terminal emulator for Emacs. It's a fully featured term and runs your favorite shell (zsh, for me).
;; At the same time, it's within Emacs, so it integrates with your normal Emacs life.

;;; Code:

(use-package vterm
  :ensure t
  :bind
  ("C-x M-t" . vterm)
  :config
  ;; This code allows sets up message passing so that when you change directory inside vterm,
  ;; emacs also updates your working directory (so that find-file starts from the right place, e.g.)
  ;; It is required to also add some stuff to your shell config.
  ;; see https://github.com/akermu/emacs-libvterm
  ;; The recommended way seems to be modifying the prompt to print some message, but that didn't
  ;; work for me, so instead I use this method.
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
  )
;; TODO Help with renaming vterms
;; I almost always rename terminal buffers (using rename-buffer), typically to ${something}-terminal
;; It would be nice to have some easy way to do this. Maybe just a thin wrapper around `vterm' which
;; does asks for a name.

(provide 'init-term)
;;; init-term.el ends here
