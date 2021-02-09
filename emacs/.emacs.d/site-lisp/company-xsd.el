;;; company-xsd.el --- Company-mode backend for xml files that relies on xsd schema. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Erik Nikko

;; Package-Version: 0.4.0

;; Author: Erik Nikko <65210465+enikko@users.noreply.github.com>
;; Created: 2020-08-06
;; Keywords: Autocompletion, company-mode backend, xml, xsd

;; This file is not part of GNU Emacs
;; The file is free software available under BSD 3-clause license (see LICENSE).
;; No warranties are provided.

;;; Commentary:
;; This file contains the company backend that selects the available completions
;; that are available in the current context.
;; See README.md for more information

;;; Change Log:
;; * Add support for symbols with ":" (such as tags with namespaces).

;;; Code:

(require 'cl-lib)
(require 'xsd)
(require 'company)
(require 'company-template)

(defun company-xsd--safe-schemas-p (schemas)
  "Non-nil if all SCHEMAS are safe (alist of namespace and paths)."
  (seq-every-p (lambda (schema) (and (consp schema)
                                     (or (eq (car schema) nil)
                                         (stringp (car schema)))
                                     (stringp (cdr schema))))
               schemas))

(defun company-xsd--safe-namespace-qualifiers-p (namespace-qualifier)
  "Non-nil if all NAMESPACE-QUALIFIER are safe (alist of namespace and qualifier)."
  (seq-every-p (lambda (ns-qual) (and (consp ns-qual)
                                      (stringp (car ns-qual))
                                      (or (eq (cdr ns-qual) nil)
                                          (stringp (cdr ns-qual)))))
               namespace-qualifier))

(defcustom company-xsd-schemas nil
  "Override the xsd schema describing the file.

Schemas are specified on the form (namespace . uri) and
an unqualified namespace is specified as nil.  For example
\((\"http://www.w3.org/2001/XMLSchema-instance\" .
   \"http://www.w3.org/2001/XMLSchema-instance.xsd\"))

If this is nil, then the company-xsd-guess-schemas function is
called to deterimine the schema for the files."
  :type '(repeat (cons (restricted-sexp :match-alternatives (string 'nil)) string))
  :safe 'company-xsd--safe-schemas-p)

(defcustom company-xsd-namespace-qualifier-alist nil
  "Override the qualifiers used for different namespaces in the file.

A namespace to qualifier mapping is described as (namespace . qualifier) where
an unqualified namespace is specified with qualifier nil.  For example
\((\"http://www.w3.org/2001/XMLSchema-instance\" . \"xsi\"))

If this is nil, then company-xsd-guess-namespace-qualifier-is called to
determine the namespace qualifiers for the file."
  :type '(repeat (cons string (restricted-sexp :match-alternatives (string 'nil))))
  :safe #'company-xsd--safe-namespace-qualifiers-p)


(defvar company-xsd--buffer-schemas nil
  "The schemas that are used for this buffer.")

(defvar company-xsd--buffer-namespace-qualifier-alist nil
  "The namespace qualifiers used for this buffer.")

(defcustom company-xsd-insert-deducible t
  "Insert all deducible information when completing."
  :type '(choice (const :tag "off" nil)
                 (const :tag "on" t)))

(defvar company-xsd--xsd-compilation-frame nil
  "The compilation frame for the current file.")

(defun company-xsd--to-full-uri (potential-uri)
  "Get POTENTIAL-URI as an uri string."
  (if (not (url-type (url-generic-parse-url potential-uri)))
      ;; No type attribute => assume relative path to the current file
      (concat "file://" (file-truename default-directory) potential-uri)
    ;; This is an URI simply return it
    potential-uri))

(defun company-xsd--using-schema-full-uri (schemas)
  "Get SCHEMAS but with full uri instead of potential relative."
  (let (res)
    (dolist (schema schemas)
      (add-to-list 'res `(,(car schema) . ,(company-xsd--to-full-uri (cdr schema)))))
    res))

(defun company-xsd-guess-schemas (xml-dom)
  "Guess the schemas (and namespaces) for the current file based on XML-DOM .

Redefine this function for non-standard lookup for schemas.
Only called if company-xsd-schemas is not set to nil."
  (let ((attributes (dom-attributes xml-dom))
        location-prefix schema-location-id schema-location-no-namespace-id
        schemas)
    ;; Get all namespaces
    (dolist (attr attributes)
      (when (and (string-match-p "xmlns\\(:.*\\)?" (symbol-name (car attr)))
                 (equal (cdr attr) "http://www.w3.org/2001/XMLSchema-instance"))
        (setq location-prefix (if (string-match-p ":" (symbol-name (car attr))) (car (cdr (split-string (symbol-name (car attr)) ":"))) nil))))

    (if location-prefix
        (progn
          (setq location-prefix (concat location-prefix ":"))
          (setq schema-location-id (concat location-prefix "schemaLocation"))
          (setq schema-location-no-namespace-id (concat location-prefix "noNamespaceSchemaLocation")))
      (setq schema-location-id "schemaLocation")
      (setq schema-location-no-namespace-id "noNamespaceSchemaLocation"))
    ;; go-through all the schema files and merge to a global completion schema
    (dolist (attr attributes)
      (when (equal (symbol-name (car attr)) schema-location-id)
        (let ((entries (split-string (cdr attr)))
              namespace location)
          (while entries
            (setq namespace (pop entries))
            (setq location (company-xsd--to-full-uri (pop entries)))
            (add-to-list 'schemas `(,namespace . ,location)))))
      (when (equal (symbol-name (car attr)) schema-location-no-namespace-id)
        (dolist (location (split-string (cdr attr)))
          (add-to-list 'schemas `(nil . ,(company-xsd--to-full-uri location))))))
    schemas))

(defun company-xsd-guess-namespace-qualifier (xml-dom)
  "Guess the namespace qualifiers for the current file based on XML-DOM .

Redefine this function for non-standard lookup for schemas.
Only called if company-xsd-schemas is not set to nil."
  (let ((attributes (dom-attributes xml-dom))
        namespaces)
    ;; Get all namespaces
    (dolist (attr attributes)
      (when (string-match-p "xmlns\\(:.*\\)?" (symbol-name (car attr)))
        (add-to-list 'namespaces `(,(cdr attr) . ,(if (string-match-p ":" (symbol-name (car attr))) (car (cdr (split-string (symbol-name (car attr)) ":"))) nil)))))
    namespaces))


(defun company-xsd--setup-backend ()
  "Set up the backend."
  (when company-xsd--buffer-schemas
    (make-local-variable 'company-xsd--xsd-compilation-frame)
    (dolist (schema company-xsd--buffer-schemas)
      (setq company-xsd--xsd-compilation-frame
            (xsd-merge-frames company-xsd--xsd-compilation-frame (xsd-compile-uri (cdr schema)))))
    (setq company-xsd--xsd-compilation-frame
          (xsd-interpret-compilation-frame company-xsd--xsd-compilation-frame
                                           company-xsd--buffer-namespace-qualifier-alist))))

(defun company-xsd--inside-tag-definition-p ()
  "Non-nil if the inside a tag definition."
  (let ((pos (point))
        (in-double-string nil)
        (in-single-string nil)
        (inside t)
        (done nil))
    (save-excursion
      (when (search-backward "<" nil 1)
        (goto-char (1+ (point)))
        (while (and (not done)
                    (< (point) (point-max))
                    (< (point) pos))
          (let ((current (string (char-after))))
            (cond
             ((equal current "<")
              (setq done t))
             ((and (not in-double-string)
                   (not in-single-string)
                   (equal current ">"))
              (setq inside nil)
              (setq done t))
             (in-double-string
              (when (equal current "\"")
                (setq in-double-string nil)))
             (in-single-string
              (when (equal current "'")
                (setq in-single-string nil)))
             ((equal current "\"")
              (setq in-double-string t))
             ((equal current "'")
              (setq in-single-string t)))
            (goto-char (1+ (point)))))))
    inside))

(defun company-xsd--completion-type ()
  "Get the completion type.

company-xsd--current-tag-name when completing a currently written tag.
company-xsd--new-tag-name when completing a not yet written tag.
company-xsd--attr when completing an attribute."
  (let* ((inside (company-xsd--inside-tag-definition-p))
         (current-point (point))
         (in-name
          (and
           inside
           (progn
             (save-excursion
               (save-restriction
                 (widen)
                 (search-backward "<" nil 1)
                 (string-match-p (replace-regexp-in-string "w" xmltok-ncname-regexp "\\`<\\(w:\\)?w?\\'" t t)
                                 (buffer-substring-no-properties (point) current-point))))))))
    (cond
     (in-name 'company-xsd--current-tag-name)
     (inside 'company-xsd--attr)
     (t 'company-xsd--new-tag-name))))

(defun company-xsd--current-completion-context (completion-type)
  "Get the current context of the xml-region.

COMPLETION-TYPE is the type of completion."
  (let ((path nil)
        (current-point (point)))
    (save-excursion
      (save-restriction
        (widen)
        (cl-case completion-type
          ('company-xsd--current-tag-name
           (search-backward "<" nil 1))
          ('company-xsd--attr
           (search-backward "<" nil 1)
           (let ((tag-start (1+ (point))))
             (save-excursion
               (re-search-forward (replace-regexp-in-string
                                   "w" xmltok-ncname-regexp
                                   "\\(w:\\)?w" t t))
               (setq path (cons (buffer-substring-no-properties tag-start (point)) path))))))
        (while (and (< (point-min) (point))
                    (condition-case nil
                        (progn
                          (nxml-backward-up-element)
                          t)
                      (error nil)))
          (setq path (cons (xmltok-start-tag-qname) path)))
        path))))
  
(defun company-xsd--get-completions ()
  "Gets the possible completions based on the current path."
  (when company-xsd--xsd-compilation-frame
    (let* ((completions '())
           (completion-type (company-xsd--completion-type))
           (completion-context (company-xsd--current-completion-context completion-type))
           candidates)
      (when (and completion-context
                 (not (equal completion-type 'company-xsd--attr)))
        (let ((qname (car (last completion-context))))
          (add-to-list 'completions (propertize (concat "</" qname ">") :qname qname))))
      (dolist (object (xsd-get-object
                       company-xsd--xsd-compilation-frame
                       completion-context))
        (setq candidates nil)
        (if (eq completion-type 'company-xsd--attr)
            (when (xsd-attribute-p object)
              (setq candidates `(,(propertize (xsd-get object :name)
                                              :type 'attribute
                                              :path completion-context
                                              :qname (xsd-get object :name)))))
          (when (xsd-element-p object)
            (setq candidates `(,(propertize (concat "<" (xsd-get object :name) ">")
                                            :type 'element
                                            :path completion-context
                                            :qname (xsd-get object :name))
                               ,(propertize (concat "<" (xsd-get object :name) "/>")
                                            :type 'element
                                            :path completion-context
                                            :qname (xsd-get object :name))))))
        (dolist (candidate candidates)
          (add-to-list
           'completions
           (propertize candidate :meta (xsd-get object :meta)
                       :doc (xsd-get object :doc)))))
      completions)))

(defun company-xsd--make-template (begin end)
  "Set region BEGIN END to a template."
  (let ((templ (company-template-declare-template begin end))
        first
        none-empty)
    (goto-char begin)
    (while (< (point) end)
      (when (and (re-search-forward "\\(=\"\\|>\\)" end 1) (not (equal (point) end)))
        (cond
         ((equal (string (char-before)) "\"")
          ;; In attribute
          (setq first (point))
          ;; TODO: handle default value inside the attribute
          (company-template-add-field templ first (point) nil))
         ((equal (string (char-before)) ">")
          ;; In opened tag
          (unless (equal (string (char-after)) "<")
            (re-search-forward "$" end t)
            (forward-char 1)
            (re-search-forward "$" end t))
          (company-template-add-field templ (point) (point) nil)
          (forward-char 1))
         (t (error (format "Unknown match when making template %s" (char-before)))))
        (setq none-empty t)))
    (if none-empty
        (company-template-move-to-first templ)
      (company-template-remove-template templ)
      (goto-char end))))

(defun company-xsd--insert-required (candidate-element)
  "Insert all required components of CANDIDATE-ELEMENT."
  (let ((candidates (xsd-get-object company-xsd--xsd-compilation-frame
                                    (append (get-text-property 0 :path candidate-element)
                                            `(,(get-text-property 0 :qname candidate-element)))))
        inserted
        start-of-tag
        closed)
    (backward-char 1)
    (when (equal (string (char-before)) "/")
      (setq closed t)
      (backward-char 1))
    (dolist (candidate candidates)
      (when (and (xsd-attribute-p candidate)
                 (eq (xsd-get candidate :usage) :required))
        (when (not (equal (string (char-before)) " "))
          (insert " "))
        (insert (xsd-get candidate :name))
        (insert "=\"\"")
        (setq inserted t)))
    (search-forward ">")
    (when (not closed)
      (let ((on-new-line (save-excursion
                           (search-backward "<")
                           (setq start-of-tag (point))
                           (re-search-backward "^")
                           (string-match-p "^[:blank:]*" (buffer-substring-no-properties (point) start-of-tag)))))
        (when on-new-line
          (insert "\n")
          (indent-according-to-mode)
          (insert "\n"))
        (insert "</" (get-text-property 0 :qname candidate-element) ">")
        (when on-new-line
          (indent-according-to-mode))))
    inserted))

(defun company-xsd--insert-deducible (candidate)
  "Insert deducible information from the inserted CANDIDATE."
  (when company-xsd-insert-deducible
    (let ((begin (point)))
      (when (eq (get-text-property 0 :type candidate) 'attribute)
        (insert "=\"\"")
        (company-xsd--make-template begin (point)))
      (when (eq (get-text-property 0 :type candidate) 'element)
        (when (company-xsd--insert-required candidate)
          (company-xsd--make-template begin (point)))))))

(defvar company-xsd--grab-symbol-syntax-table nil
  "The syntax table used to grab a symbol.")

(defun company-xsd--init-grab-symbol-syntax-table ()
  "Initialize the grab symbol syntax table."
  (when (and (not company-xsd--grab-symbol-syntax-table)
             (eq major-mode 'nxml-mode))
    (setq company-xsd--grab-symbol-syntax-table
          (make-syntax-table nxml-mode-syntax-table))
    (modify-syntax-entry ?: "_" company-xsd--grab-symbol-syntax-table)))

(defun company-xsd--grab-symbol ()
  "Gets the current symbol that is begin completed."
  (with-syntax-table company-xsd--grab-symbol-syntax-table
    (let ((guess (company-grab-symbol)))
      (unless guess
        (when (or
               ;; Can complete attributes when at end of self-closing tag
               (equal (string (char-after)) "/")
               ;; Can complete attributes when inside a tag
               (and (equal (string (char-after)) ">")
                    (not (equal (string (char-before)) "/"))))
          (insert " ")
          (backward-char 1)
          (setq guess (company-grab-symbol))
          (delete-forward-char 1)))
      (when (string= guess ">")
        ;; Nothing to complete with a tag closer, go on with new tag/attribute
        (setq guess ""))
      guess)))

(defvar company-xsd--initialized nil
  "Non-nil if company-xsd has been initialized.")

(defun company-xsd-init-buffer ()
  "Initialize company xsd for the current buffer."
  (interactive)
  (company-xsd--init-grab-symbol-syntax-table)
  (when (or (not company-xsd--initialized) (called-interactively-p))
    (hack-local-variables)
    (let ((xml-dom (if (or (not company-xsd-schemas) (not company-xsd-namespace-qualifier-alist))
                       (condition-case nil
                           (xml-parse-region (point-min) (point-max))
                         (error nil))
                     nil)))
      (setq-local company-xsd--buffer-schemas
                  (if company-xsd-schemas
                      (company-xsd--using-schema-full-uri company-xsd-schemas)
                    (company-xsd-guess-schemas xml-dom)))
      (setq-local company-xsd--buffer-namespace-qualifier-alist
                  (if company-xsd-namespace-qualifier-alist
                      company-xsd-namespace-qualifier-alist
                    (company-xsd-guess-namespace-qualifier xml-dom))))
    (company-xsd--setup-backend)
    (setq-local company-xsd--initialized t)))

(defun company-xsd-backend (command &optional arg &rest ignored)
  "The company-xsd company-mode backend.

If called interactively, then the backend forces a completion.
COMMAND and ARG is set by company-mode and the rest of the arguments
from the mode are IGNORED."
  (interactive '(interactive))
  (cl-case command
    ;; Let the user specifically call this backend
    (interactive (company-begin-backend 'company-xsd-backend))
    ;; Setup the backend
    (init
     (when (eq major-mode 'nxml-mode)
       (company-xsd-init-buffer)))
    ;; When to run the backend and what is the prefix
    (prefix (and (eq major-mode 'nxml-mode)
                 (company-xsd--grab-symbol)))
    ;; Which are the completion candidates
    (candidates (cl-remove-if-not (lambda (c) (or (not arg) (string-prefix-p arg c)))
                                  (company-xsd--get-completions)))
    ;; Get the inline extra info for the candidate (in drop-down)
    (annotation (concat " " (get-text-property 0 :meta arg)))
    ;; Get the documentation for the candidate (in minibar)
    (meta (get-text-property 0 :doc arg))
    ;; Called after completing the candidate (perfect place to insert extra text)
    (post-completion (company-xsd--insert-deducible arg))))

(provide 'company-xsd)
;;; company-xsd.el ends here
