;;; jiq.el --- run jq interactively inside emacs
;; Copyright (C) 2020-2020 Taylor Sutton
;; Author: Taylor Sutton <tsutton125@gmail.com>
;; Created: 2020-02-22
;; Version: 0.1.0


;;; Commentary:
;; Work in Progress!

;;; Code:

;; Plans for getting into interactive-command mode
;;    "mode-on-current-buffer"
;;    "mode-on-buffer"
;;    "mode-on-file"
;;    "mode-on-region"

;; Major mode TODO
;; - defcustom for auto execute when typing stops: Probably https://www.gnu.org/software/emacs/manual/html_node/elisp/Command-Loop.html#Command-Loop
;; - ability to set flags for jq
;; - view jq man page
;; - split error output and put it in a split window with previous output?

(defun jiq-run-jq (filter input-buffer output-buffer)
  "Run jq 'FILTER' on INPUT-BUFFER, overwriting OUTPUT-BUFFER with the results."
  (with-current-buffer output-buffer
    (kill-region (point-min) (point-max))
    )
  (with-current-buffer input-buffer
    (call-process-region
     (point-min) (point-max)
     "jq"
     nil
     output-buffer
     nil
     filter
     )
    )
  )

(defun jiq-run-jq-from-buffer ()
  "Like jiq-run-jq, but read the filter as the contents of current buffer."
  (interactive)
  (jiq-run-jq (buffer-string) jiq-source-buffer jiq-view-buffer)
  )

(defun jiq-on-buffer (buffer)
  "Start JQ mode on the contents of BUFFER."
  (interactive "bInput buffer: ")
  (let (
        (view-buffer (generate-new-buffer "*jiq-view*"))
        (command-buffer (generate-new-buffer "*jiq-cmd*"))
        )
    (with-current-buffer command-buffer
      (jiq-mode)
      (setq-local jiq-view-buffer view-buffer)
      (setq-local jiq-source-buffer buffer)
      (insert ".")
      (jiq-run-jq-from-buffer)
      )
    (with-current-buffer view-buffer
      (json-mode)
      )
    (let* (
           (window (display-buffer view-buffer))
           (cmd-window (split-window window -4 'above))
           )
      (set-window-buffer cmd-window command-buffer)
      (select-window cmd-window)
      )
    )
  )

;; JQ mode map TODO:
;; - bindings for shirnk-window and enlarge-window
;; - convenience shortcuts/snippets? Like map(<point>), use smartparens with it
;; - (stretch) completion for ".<TAB>"

(defvar jiq-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'jiq-run-jq-from-buffer)
    map)
  )

(define-derived-mode jiq-mode fundamental-mode "JIQ"
  "documentation goes here"

  )

(provide 'jiq)
;;; jiq.el ends here
