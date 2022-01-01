;;; init-elisp.el --- Configuration for Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;;

;;; Code:

(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'emacs-lisp-byte-compile-and-load)

(provide 'init-elisp)
;;; init-elisp.el ends here
