;;; init-completion.el --- Configure completion for user input -*- lexical-binding: t; -*-
;;; Commentary:

;; Completion in Emacs refers to providing completions for user input, typically via the mini-buffer.
;; For example, when opening a file via find-file, switching buffers switch-to-buffer, etc.
;; (Also, company-mode uses a little bit of the Emacs completion system.)
;; In typical Emacs fashion, completion is highly customizable, and since this is one of the more involved
;; and preference-oriented configs I have, I'm writing some longer-form documentation to explain how
;; it works, what some options are, and what I use.

;; Emacs has two main knobs in its API for customizing completion: the function completing-read and
;; the variable completion-styles. completing-read reads an input from the minibuffer, but not just that,
;; it also can show a list of candidates for the input which can change dynamically based on what the
;; user has input so far.
;; completion-styles controls the way the partial user input is used to filter the full list of candidates.
;; For example, a completion style might allow user input as a regex. Or it might require the partial input to
;; match the beginning of the candidate, or any part of the candidate, etc.
;; The Emacs documentation has more, for example:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Minibuffer-Completion.html
;; Using these APIs, completion can be customized for any function in any package that uses completing-read.

;; On the other hand, another way to customize the completion of find-file is to remap find-file to my-find-file
;; which can do arbitrary completion. This is more flexible and powerful, but more fragile and trickier.

;; Emacs ships with ido-mode, which uses the second approach (it provides an ido-completing-read,
;; then remaps find-file to ido-find-file which uses ido-completing-read), as well as icomplete-mode which
;; uses just the standard APIs. For a long time I used ido-mode.

;; We can roughly divide packages in this space into backends, frontends, and frameworks. Sometimes these
;; compose with each other, and a given frontend or framework uses a specific backend.


;; Here's the landscape of packages in this area, as I see it.

;; For completion backends (filtering) there is mainly Orderless and Prescient.
;;
;; Orderless is almost purely a completion style that can be used with completion front-ends.
;; It works out-of-the-box with frontends that respect completion-styles, e.g. ido, vertico, selectrum, icomplete-vertical.
;; It has several sub-styles, including regex, literal, initialism, and it divides the input into space-separated
;; components such that all must match.
;;
;; Prescient is pretty similar to orderless, except that it also provides sorting and includes both history and
;; how good the match is in sorting. This makes it more powerful, except traditionally sorting is by the frontend,
;; not the style, so it requires special integration packages such as ivy-prescient and company-prescient to integrate.

;; For frontends, there are several options, the main ones being:
;; - icomplete-vertical: relatively light-weight add-on to icomplete to improve its UI
;; - selectrum
;; - vertico
;; The latter two are very similar, both providing enhancements using the completing-read interface.
;; Selectrum uses a more complex method of enhancement, overriding some functions and adding advice to others,
;; that make it compose slightly less well with other packages. But both support orderless and prescient, and
;; both are supported by consult (see below).

;; For frameworks, there are three main ones:
;; - Helm
;; - Ivy + Counsel + Swiper
;; - Consult (+Embark + Marginalia)
;; Helm and Ivy/Counsel are powerful completion systems that replace the builtins like find-file with their own
;; Each has its own ecosystems of adapters which integrate various commands that read user input into the
;; respective completion system. Ivy pairs with Swiper and Counsel to provide this (ivy is the completion system)
;; (swiper and counsel provide the actual commands like counsel-find-file and counsel-M-x; swiper is isearch+ivy).
;; Neither of them try to compose well with completion APIs, but because they are powerful and popular,
;; there are frequently adapters (e.g. prescient can be used with ivy, although not helm)
;; OTOH, Consult provides nicer versions of the built-ins like find-file in a way that completely defers completion
;; to completing-read. So, it has compatibility with vertico, selectrum, built-ins, and more.
;; One powerful aspect of Ivy and Helm is the notion of "actions": for find-file, after entering enough input to
;; select the file in the minibuffer, you can apply a non-default action to the file - the default being visit in buffer.
;; But additional actions might include renaming the file, deleting it, etc.
;; In the Consult world, these contextual actions provided by Embark. Embark does a lot, but its deep integration
;; with Consult is a highlight.

;; Personally, I don't really value the composability aspect, I value the final resulting UI that I get.
;; Plus, the compasbile parts still have a bunch of extension packages to install.
;; I've used Ivy for a while now, I tried helm and it didn't really speak to me. Now I'm trying Consult+Embark.

;;; Code:

;; TODO install consult-flycheck, consult-lsp (and maybe others  from https://github.com/minad/consult#recommended-packages)
;; TODO explore mct: https://gitlab.com/protesilaos/mct
;; TODO explore corfu, CAPE
;; TODO I'm going to miss counsel+projectile's actions on project, I wonder if there's an embark thing
;; TODO consult's yanking or browse-kill-ring?
;; TODO customize orderless with dispatchers, to make regex opt-in, add excludes, etc:
;;      https://old.reddit.com/r/emacs/comments/kqutap/selectrum_prescient_consult_embark_getting_started/
;;      With this, I'd be happier with orderless than prescient, o/w I feel prescient might edge it out
;; TODO is consult-line good as an isearch replacement?

;; Can be "ivy" or "vertico"
;; Ivy means Ivy+Swiper+Counsel+Prescient
;; Vertico means Orderless+Vertico+Consult+Embark+Marginalia
(setq ts/completion-stack "vertico")

(use-package ivy
  :if (string= ts/completion-stack "ivy")
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
  :if (string= ts/completion-stack "ivy")
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
  :if (string= ts/completion-stack "ivy")
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
  :if (string= ts/completion-stack "ivy")
  :ensure t
  :config
  (prescient-persist-mode)
  (setq prescient-sort-full-matches-first t)
  )

(use-package ivy-prescient
  :if (string= ts/completion-stack "ivy")
  :ensure t
  ;; according to docs, must be loaded after counsel, since both modify ivy
  :after counsel
  :diminish
  :config
  (ivy-prescient-mode)
  )

(use-package company-prescient
  :if (string= ts/completion-stack "ivy")
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
  :if (string= ts/completion-stack "ivy")
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (counsel-projectile-mode 1)
  )

(use-package vertico
  :if (string= ts/completion-stack "vertico")
  :init
  (setq enable-recursive-minibuffers t)

  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (vertico-mode)
  )

(use-package orderless
  :if (string= ts/completion-stack "vertico")
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
	;; The docs say something aobut how partial-completion lets you deal with multiple files...
	;; Not sure how that works
        completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  :init
  (marginalia-mode))

;; This block is copy-pasted from the consult README, with some bits commented out while I explore.
(use-package consult
  :if (string= ts/completion-stack "vertico")
  :bind (;; C-c bindings (mode-specific-map)
         ;; ("C-c h" . consult-history)
         ;; ("C-c m" . consult-mode-command)
         ;; ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ;; Custom M-# bindings for fast register access
         ;; ("M-#" . consult-register-load)
         ;; ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ;; ("C-M-#" . consult-register)
         ;; Other custom bindings
         ;; ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ;; ("M-g e" . consult-compile-error)
         ;; ("M-g f" . consult-flycheck)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ;; ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading TODO
         ;; ("M-g m" . consult-mark)
         ;; ("M-g k" . consult-global-mark)
         ;; ("M-g i" . consult-imenu)
         ;; ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ;; ("M-s d" . consult-find)
         ;; ("M-s D" . consult-locate)
         ;; ("M-s g" . consult-grep)
         ;; ("M-s G" . consult-git-grep)
         ;; ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ;; ("M-s L" . consult-line-multi)
         ;; ("M-s m" . consult-multi-occur)
         ;; ("M-s k" . consult-keep-lines)
         ;; ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ;; ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  (recentf-mode +1) ;; Not sure if this is the best place for it

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
)

;; TODO on an identifier, there's xref-find-definitions, but not find-references
(use-package embark
  :if (string= ts/completion-stack "vertico")
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :if (string= ts/completion-stack "vertico")
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(defun ts/vterm-in-project (project-root-dir)
  "Open a vterm in the project PROJECT-ROOT-DIR."
  (interactive "D")
  (let ((default-directory project-root-dir))
    (call-interactively 'projectile-run-vterm)
    )
  )

(defun ts/magit-in-project (project-root-dir)
  "Open magit-status in the project PROJECT-ROOT-DIR."
  (interactive "D")
  (let ((default-directory project-root-dir))
    (funcall-interactively 'magit-status project-root-dir)
    )
  )

;; With this, consult-projectile is a combo projectile-find-file, projectile-switch-to-buffer, and projectile-switch-project.
;; And it adds metadata to those for Marginalia and Embark to use.
(use-package consult-projectile
  :after embark-consult
  :commands consult-projectile
  :config

  (embark-define-keymap embark-project-map
    "Keymap for actions on projectile projects."
    ("v" ts/vterm-in-project)
    ("g" ts/magit-in-project)
    )

  ;; consult-projectile adds a defvar consult-projectile--source-projectile-project which
  ;; is vertico/consult source backed by projectile-relevant-known-projects as candidates and
  ;; with the consult-projectile-project category. The candidate list is a path to the root of the
  ;; project
  ;; consult-projectile uses ":category 'consult-projectile-project"
  ;; I'm not exactly sure what that is (an atom beginning with ' ?)
  ;; But I couldn't get embark-keymap-alist to recognize it
  ;; for now, I'm using a locally modified consult-projectile which changes it to
  ;; 'category 'consult-projectile-project
  ;; TODO can I make this work without modifying consult-projectile?
  (add-to-list 'embark-keymap-alist '(consult-projectile-project . embark-project-map))
  )



(provide 'init-completion)
;;; init-completion.el ends here
