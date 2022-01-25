;;; init-structured-text.el --- TODO -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; Code:

(use-package yaml-mode
  :mode "\\.yml\\'")

(use-package highlight-indent-guides
  :hook (yaml-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  )

(use-package json-mode)

(provide 'init-structured-text)
;;; init-structured-text.el ends here
