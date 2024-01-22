;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'cl-macs)
(require 'apache-c-style)

(defvar asoc-compare-fn 'eq
  "Special variable holding the equality predicate used in asoc functions.

May take the values `equalp' (or `cl-equalp'), `equal', `eql', `eq'. When unset,
functions default to using `equal'.

This variable may be passed to asoc functions dynamically in a let binding.")

(defun asoc---assoc (key alist)
  "Return the first association in ALIST matching KEY, or else nil.

The equality test to be used is determined by `asoc-compare-fn'."
  (cl-case asoc-compare-fn
    ((equal nil)  (assoc key alist))
    (t (progn
         (while (and alist
                     (not (funcall asoc-compare-fn (caar alist) key)))
           (setf alist (cdr alist)))
         (car alist)))))


(defun asoc---uniq (alist)
  "Return a copy of ALIST with duplicate keys removed.

The first pair with a given key is kept, later pairs are omitted. Key equality
is determined using `asoc-compare-fn'."
  (let ( result
         (rest alist) )
    (while rest
      (let* ((pair  (car rest))
             (key   (car pair)))
        (unless (asoc---assoc key result)
          (push pair result)))
      (setq rest (cdr rest)))
    (nreverse result)))

(defun asoc-merge (&rest alists)
  "Return an alist with unique keys resulting from merging ALISTS.

When identical keys occur in two alists, the latter alist takes precedence.
When identical keys occur within a single alist, the foremost takes precedence
(as usual).

With a single argument, equivalent to `asoc-uniq'.

Example:

    (asoc-merge '((a . 1) (b . 2) (a . 4))
                '((a . 4) (c . 5) (c . 6)))
    ;; ((a . 4) (c . 5) (b . 2))"
  (asoc---uniq (apply #'append (nreverse alists))))

(defconst fluent-bit-c-style--defaults
  '((fill-column . 90)))

(defconst fluent-bit-c-style (asoc-merge apache-c-style fluent-bit-c-style--defaults))

(c-add-style "fluent-bit-c" fluent-bit-c-style)

(provide 'fluent-bit-c-style)
;;; fluent-bit-c-style.el ends here
