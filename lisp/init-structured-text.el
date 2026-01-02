;;; init-structured-text.el --- TODO -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;; Code:

(use-package yaml-mode
  :ensure t
  :mode "\\.yml\\'")

(use-package yaml
  :ensure t)
(use-package yaml-pro
  :ensure t)

(use-package highlight-indent-guides
  :ensure t
  :hook (yaml-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  )

(use-package json-mode
  :ensure t
  )

(provide 'init-structured-text)
;;; init-structured-text.el ends here
