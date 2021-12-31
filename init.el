;;; init.el --- Load my emacs config
;;; -*- lexical-binding: t; -*-
;;; Commentary:

;; This is my Emacs configuration.
;; ...

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialize package-related things.
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq straight-use-package-by-default t)

(straight-use-package 'use-package)

;; benchmark-init can be used to get timings of initialization
;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; out-of-band: install use-package from MELPA e.g M-x package-install RET use-package RET
(eval-when-compile
  (require 'use-package)
  ;; uncomment this to get messages on package load, and timings if a load is slow
  ;; which in particular can tell you what's NOT deferred (because it loads on startup)
  ;; (setq use-package-verbose t)
  )

;;  use-package is only needed at compile time, but in order for
;; the :bind keyword to work, bind-key is needed at runtime
(require 'bind-key)

;; Before we go on setting up a bunch of global minor modes and whatnot
;; let's install diminish so we can hide them from the modeline
(use-package diminish
  :ensure t)

;; NOTE: Most, but not all, modes are declared by use-package
;; Some modes that aren't configured in any special way are just listed under
;; custom package-selected-packages variable

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

(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text editing (search, navigation, completion)

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

;; Highlight the line that point is located on! Nifty little mode.
(global-hl-line-mode +1)

;; I don't have a lot of custom snippets, but the builtin set is pretty good,
;; plus it integrates with lsp-mode for servers to provide their own snippets.
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode)
  )

;; Anzu provides incremental improvements to query-replace(-regexp)
;; It shows how many matches there are for the query. the anzu variant highlights
;; the text being replaced (as does builtin query-replace), but anzu also shows
;; the text that will be inserted, and it highlights matches while still typing
;; the query (as opposed to builtin, which only highlights after pressing RET on
;; the query). This is especially handy for regexp, and especially for regexp
;; where the replacement has refs to groups matched by the regexp
(use-package anzu
  :ensure t
  :diminish
  :config
  (global-anzu-mode)
  (global-set-key (kbd "C-M-a") 'anzu-query-replace) ;formerly beginning-of-defun
  (global-set-key (kbd "C-M-e") 'anzu-query-replace-regexp) ;formerly end-of-defun
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


;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;; Don't disable case-change functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; set-goal-column is mostly useful with keyboard macros
(put 'set-goal-column 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General programming config (lsp, symbol completion, linting, project management, etc)
(use-package flycheck
  :ensure t
  :diminish
  :config
  (global-flycheck-mode)
  )

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
  (require 'dap-go)
  (dap-go-setup)
  )


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
  :config
  (setq projectile-completion-system 'ivy)
  ;; The projectile mode line is too long! I don't need to know the type of
  ;;  project, I likely already know that immediately from the name.
  (defun projectile-custom-mode-line ()
    (let ((project-name (projectile-project-name)))
      (format " proj:%s"
              (or project-name "-"))))
  (setq projectile-mode-line-function 'projectile-custom-mode-line)
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

;; hl-todo-mode provides highlighting for TODO and NOTE and FIXME (by default, it's configurable)
(use-package hl-todo
  :ensure t
  :diminish
  :config
  (global-hl-todo-mode)
  )

(global-set-key (kbd "M-[") 'beginning-of-defun)
(global-set-key (kbd "M-]") 'end-of-defun)

;; rainbow-delimiters makes parens, brackets, etc different colors depending in nesting depth
;; It's a bit subtle so I don't get much value of it, but it's better than nothing.
;; The colors *are* configurable, but eh.
(use-package rainbow-delimiters
  :ensure t
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;; ==from prelude==
;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; ==from prelude== (modified to use use-package)
;; diff-hl shows when a part of a version-controlled file has been modified by changing
;; the color of the window's fringe. It also provides commands to see the difference and
;; to jump between these modified hunks, but I haven't integrated those into my flow.
(use-package diff-hl
  :ensure t
  :diminish
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Various special-mode configs

;; Rust configuration
(use-package rustic
  :ensure t
  :defer t
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  ;; defer formatting to lsp-mode
  (setq rustic-format-on-save nil)
  ;; The default is this WITHOUT the `-D warnings'.
  ;; I started adding -D warnings since that's what the default/quickstart CI configuration for Rust does.
  (setq rustic-flycheck-clippy-params "--message-format=json -Zunstable-options -- -D warnings")

  ;; there are tons of LSP settings we might want to set, see
  ;; https://emacs-lsp.github.io/lsp-mode/page/lsp-rust/#lsp-rust-analyzer-cargo-watch-command
  ;; https://rust-analyzer.github.io/manual.html
  ;; The default behavior of rust-analyzer is to awkwardly merge imports and merge-y as possible
  ;; Instead, do something sane - don't do any nested merges.
  (setq lsp-rust-analyzer-import-merge-behaviour "last")
  ;; I think this gets clippy feedback into flycheck or something, I'm not exactly sure
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")

  ;; Some things to test to improve proc macro support
  ;; setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq lsp-rust-analyzer-proc-macro-enable t )
  )
;; rustic-mode come built-in with lsp-mode integration, so no need to add a hook
;; to rustic-mode to enable LSP.

;; Go config
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; terraform

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
;; Scala

;; Enable scala-mode for highlighting, indentation and motion commands
(use-package scala-mode
  :ensure t
  :interpreter
    ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :ensure t
  :defer t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)
(use-package lsp-metals
  :ensure t
  :defer t
  :config (setq lsp-metals-treeview-show-when-views-received t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Org mode
(use-package org
  :straight nil
  :defer t
  :config
  (add-hook 'org-mode-hook 'toggle-truncate-lines)
  (setq
   org-agenda-custom-commands
   '(("x" "Un-scheduled" tags "+TODO=\"TODO\"-SCHEDULED<>\"\"-DEADLINE<>\"\"" nil)
     ("n" "Agenda and all TODOs"
      ((agenda #1="")
       (alltodo #1#))))
   org-adapt-indentation nil
   org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled
   org-agenda-skip-scheduled-if-deadline-is-shown t
   org-agenda-todo-list-sublevels nil
   org-hide-leading-stars t
   org-hierarchical-todo-statistics nil
   org-refile-allow-creating-parent-nodes 'confirm
   org-refile-targets '((org-agenda-files :maxlevel . 2))
   )
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (calc . t))
   )
  (cond
   ((equal (system-name) "quillen.local")
    (setq org-agenda-files (quote ("~/general.org")))
    (setq org-directory "~/")
    )
   (t
    (setq org-agenda-files (quote ("~/OneDrive/todo.org" "~/OneDrive/notes.org" "~/OneDrive/cs.org")))
    (setq org-directory "~/OneDrive")
    )
   )
  :bind (:map org-mode-map
	      ("C-M-j" . org-shiftmetaleft)
	      ("C-M-k" . org-shiftmetaright)
	      )
  )

(use-package org-agenda
  :straight nil
  :bind ("C-c a" . org-agenda)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unsorted config
(use-package magit
  :defer t
  :ensure t)

(use-package project)

;; forge lets you interact with things like github PRs and issues from inside magit
;; It requires external setup for creating and storing, API tokens plus setting
;; variables (e.g. github username)
;; For more info, see https://magit.vc/manual/forge/index.html#Top
(use-package forge
  :ensure t
  :after magit)

(fset 'yes-or-no-p 'y-or-n-p)

;; =from prelude=
;; (except I removed the "Prelude" part)
(setq frame-title-format
      '("" invocation-name ": " (:eval (if (buffer-file-name)
                                            (abbreviate-file-name (buffer-file-name))
                                          "%b"))))
(diminish 'eldoc-mode)
(diminish 'auto-revert-mode)

;; recentf tracks recently visited files and integrates into stuff.
(setq-default
 recentf-max-saved-items 300 ; default is 20
 recentf-max-menu-items 10 ; default is 10
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

(use-package ace-window
  :ensure t
  :commands (ace-window)
  :bind ([remap other-window] . ace-window)
  ;; Other possibly useful variables:
  ;; - aw-scope (by default it's 'global, but might want 'frame instead)
  ;; - the faces used by ace
  )

(define-key emacs-lisp-mode-map (kbd "C-c C-l") 'emacs-lisp-byte-compile-and-load)

(setq-default fill-column 100)

;; savehist keeps track of minibuffer history
(require 'savehist)
 (setq savehist-additional-variables
;;       ;; search entries
       '(search-ring regexp-search-ring))
;;       ;; save every minute
;;       savehist-autosave-interval 60
(savehist-mode +1)


;; vterm is a terminal emulator for emacs. It's a fully featured term and runs your favorite shell (zsh, for me).
;; At the same time, it's within emacs, so it integrates with your normal emacs life.
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

(add-hook 'prog-mode-hook #'subword-mode)
(add-hook 'yaml-mode-hook #'subword-mode) ; yaml-mode doesn't inherit prog-mode

;; jiq is a hacky little mode I made for using the jq command line tool, interactively
;; It is very much a WIP, but as is, it can be used for pulling data out of a JSON buffer
;; where you might not know exactly the jq filter ahead of time.
;; TODO work more on jiq mode
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'jiq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom
;; I try to only use custom-set-variables for built-ins.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'y-or-n-p)
 '(custom-safe-themes
   '("79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default))
 '(delete-active-region nil)
 '(inhibit-startup-screen t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(dap-go forge terraform-mode company-prescient ivy-prescient prescient diff-hl no-littering lsp-metals sbt-mode async scala-mode go-eldoc vterm magit lsp-mode exec-path-from-shell rainbow-delimiters doom-themes ample-theme crux json-mode yaml-mode markdown-mode go-mode dockerfile-mode anzu yasnippet hl-todo zop-to-char lsp-ui lsp-ivy browse-kill-ring smartparens undo-tree which-key avy counsel-projectile diminish swiper ivy ivy-mode company flycheck rustic use-package))
 '(require-final-newline t)
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(scroll-preserve-screen-position t)
 '(sp-highlight-pair-overlay nil)
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-separator "/"))
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
;; golang => prelude had some handy keybinds I might want to adapt
;; some dired mode config
;; prelude-emacs-lisp
;; => nifty auto-recompile on save with some smartness
;; => rainbow-mode to colorize strings like #FFFFFF according to their value
;; check out company-terraform for terraform autocompletes? plus terraform-mode and its recommended format
;; set-mark-command-repeat-pop t

;; rust notes
;; enable lsp-ui-peek-mode (what does the MODE do when I have the keybindings already?)

(provide 'init)
;;; init.el ends here
