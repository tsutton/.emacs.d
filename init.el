;;; init.el --- Load my emacs config -*- lexical-binding: t; -*-
;;; Commentary:

;; This is my Emacs configuration.
;; ...

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize package-related things.
(defvar elpaca-installer-version 0.8)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Uncomment for systems which cannot create symlinks:
(elpaca-no-symlink-mode)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

(eval-when-compile
  (require 'use-package)
  )

;; benchmark-init can be used to get timings of initialization
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; uncomment this to get messages on package load, and timings if a load is slow
;; which in particular can tell you what's NOT deferred (because it loads on startup)
;; (setq use-package-verbose t)

;;  use-package is only needed at compile time, but in order for
;; the :bind keyword to work, bind-key is needed at runtime
(require 'bind-key)

;; Before we go on setting up a bunch of global minor modes and whatnot
;; let's install diminish so we can hide them from the modeline
(use-package diminish
  :ensure t)

;; Necessary for stuff e.g. calling rg or jq from elisp.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (not (equal system-type 'windows-nt) )
    (exec-path-from-shell-initialize)
    )
  )

;; no-littering helps organize the random files that various modes use to save state
;; it is basically a giant file which does a bunch of setq to change the variables
;; the those modes use as their save location to put them under either:
;; .emacs.d/etc/ (for config-type files)
;; .emacs.d/var/ (for savestates)
;; We want to load it fairly early because otherwise, other modes may run and start
;; creating their files and such before the locations have been changed.
;; NOTE When setting up no-littering, if you want to preserve existing config/state,
;; you must manually look up where to put your existing files for them to keep working.
;; It's a hassle :/
(use-package no-littering
  :ensure t
  )

(elpaca-wait)

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-look-and-feel)
(require 'init-emacs-qol)
(require 'init-version-control)
(require 'init-generic-text)
(require 'init-org)
(require 'init-completion)
(require 'init-programming)
(require 'init-structured-text)
(require 'init-term)

(require 'init-lsp)

;; TODO these might have implicit deps on previous steps
(require 'init-rust)
(require 'init-go)
(require 'init-scala)
(require 'init-elisp)

;; jiq is a hacky little mode I made for using the jq command line tool, interactively
;; It is very much a WIP, but as is, it can be used for pulling data out of a JSON buffer
;; where you might not know exactly the jq filter ahead of time.
;; TODO work more on jiq mode
(require 'jiq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous that doesn't get its own file yet.

(use-package terraform-mode
  :ensure t
  :mode "\\.tf\\'"
  )

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-command "md2html"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;; I try to only use custom-set-variables for built-ins.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Borderline things to consider in the future

;; hippie-expand and dabbrev
;;   There is plenty of example setup for this e.g. in prelude and in purcell/emacs.d
;; Treemacs and its lsp integration
;; some dired mode config
;; prelude-emacs-lisp
;; => nifty auto-recompile on save with some smartness
;; => rainbow-mode to colorize strings like #FFFFFF according to their value
;; check out company-terraform for terraform autocompletes? plus terraform-mode and its recommended format
;; set-mark-command-repeat-pop t

;; rust notes

(provide 'init)
;;; init.el ends here
