;;; init-rust.el --- Configuration for Rust projects -*- lexical-binding: t; -*-
;;; Commentary:
;;

;;; Code:

;; rustic-mode come built-in with lsp-mode integration, so no need to add a hook
;; to rustic-mode to enable LSP.
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

(provide 'init-rust)
;;; init-rust.el ends here
