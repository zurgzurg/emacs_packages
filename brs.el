;;
;; major mode for editing / viewing brightscript
;;
;;

(defvar brs-font-lock-keywords
  '(
    "\\<function\\>\\|\\<end\\>\\|\\<return\\>\\|\\<if\\>\\|\\<then\\>\\|\\<else\\>\\|\\<as\\>\\|\\<string\\>\\|\\<object\\>\\|\\<dynamic\\>\\|\\<sub\\>\\|\\<integer\\>"
    ("function \\([a-zA-Z_][a-zA-Z0-9_]*\\)" (1 font-lock-function-name-face))
    ("sub \\([a-zA-Z_][a-zA-Z0-9_]*\\)"      (1 font-lock-function-name-face))
    )
  "Keyword highlighting specification for `sample-mode'.")

(defvar brs-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?'  "<" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?_  "_" st)
    st)
  "Syntax table for `brs-mode'.")

(define-derived-mode brs-mode fundamental-mode "Brs"
  "Major mode for brightscript files."
  :syntax-table brs-mode-syntax-table
  (setq-local comment-start "'")
  (setq-local font-lock-defaults
	'(brs-font-lock-keywords nil t)))

(setq auto-mode-alist
      (append
       '(("\\.brs\\'" . brs-mode))
       auto-mode-alist))
