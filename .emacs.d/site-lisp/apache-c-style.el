;;; package -- Summary
;;; Commentary:
;;; Code:

(defconst apache-c-gnu-indent-options '("-i4" "-npsl" "-di0" "-br" "-nce" "-d0" "-cli0" "-npcs" "-nfc1" "-nut") "Arguments for gnu indent program")

(defconst apache-c-style
  '(
    (indent-tabs-mode . nil)
    (inclass . ++)
	(defun-block-intro . ++)
	(statement-block-intro . ++)
	(substatement . ++)
	(brace-list-intro . ++)
	(statement-case-intro . ++)
	(inextern-lang . 0)))

(c-add-style "apache-c" apache-c-style)

(provide 'apache-c-style)
;;; apache-c-style.el ends here
