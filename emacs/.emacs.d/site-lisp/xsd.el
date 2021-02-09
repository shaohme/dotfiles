;;; xsd.el --- Compiler of xsd schemas. -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Erik Nikko

;; Author: Erik Nikko <65210465+enikko@users.noreply.github.com>
;; Created: 2020-08-06
;; Keywords: xsd

;; This file is not part of GNU Emacs
;; The file is free software available under BSD 3-clause license (see LICENSE).
;; No warranties are provided.

;;; Commentary:
;; This file contains a compiler that compiles a xsd schema file to elisp structures.
;; See README.md for more information

;;; Change Log:
;;; Code:

(require 'xml)
(require 'cl-lib)
(require 'url)
(require 'seq)
(require 'dom)

(defconst xsd--root-xsd-id "/[:root]"
  "The xsd-id for the root entity.")

(defun xsd-uri-fetch (uri)
  "Gets a buffer with the content of URI.

Override for specific purpose linkage.
Don't forget to clean the buffer when overriding, 'url-retrieve' clears buffers it opens."
  (with-current-buffer (url-retrieve-synchronously uri t t)
    ;; Remove headers
    (delete-region (point-min) (point))
    (current-buffer)))

(defun xsd--empty-frame ()
  "Create an empty compilation frame."
  (copy-tree `(:xsd-path ()
               :xsd-parent ()
               :xsd-xmlns-alist ()
               :xsd-target-namespace nil
               :xsd-namespace-qualification ()
               :xsd-entities ()
               :xsd-schema-qualifier nil
               :xsd-schema-uri nil)))

(defalias 'xsd-get 'plist-get)
(defalias 'xsd-set 'plist-put)

(defun xsd--print-entities (entities)
  "Print ENTITIES to current buffer."
  (let (xsd-id entity)
    (while entities
      (setq xsd-id (pop entities))
      (unless entities
        (error "XSD-ID without and entity"))
      (setq entity (pop entities))
      (princ xsd-id)
      (cl-case (xsd-get entity :tag)
        (:inline-element (princ ":"))
        (:ref-element (princ (format " reference to %s" (xsd-get entity :name))))
        (:inline-attribute nil)
        (:ref-attribute (princ (format " reference to %s" (xsd-get entity :name)))))
      (princ "\n")
      (dolist (child (xsd-get entity :children))
        (princ "  * ")
        (princ child)
        (princ "\n")))))

(defun xsd--set-schema-uri (frame uri)
  "Sets the URI of the schema in FRAME."
  (plist-put frame :xsd-schema-uri uri))

(defun xsd--as-absolute-uri (frame location)
  "Gets LOCATION as an uri based on FRAME.

If location is relative then location is relative to the
schema location in frame."
  (if (not (url-type (url-generic-parse-url location)))
      (concat (file-name-directory (plist-get frame :xsd-schema-uri)) location)
    location))

(defun xsd--human-viewable-path (frame)
  "Get the path of FRAME."
  (reverse (plist-get frame :xsd-path)))

(defun xsd--set-path (frame path)
  "Set PATH of FRAME."
  (plist-put frame :xsd-path (reverse path)))

(defun xsd--construct-id (path tag ns-name)
  "Construct a unique id from XMLNS-ALIST, a PATH, a TAG type and a NS-NAME."
  (let ((name ""))
    (dolist (path-entry path)
      (when (or (eq (car path-entry) :inline-element)
                (eq (car path-entry) :typed-element)
                (eq (car path-entry) :ref-element)
                (eq (car path-entry) :named-type))
        (setq name (concat name "/" (cdr path-entry)))))
    (concat name
            "/"
            ns-name
            "["
            (cond
             ((or (eq tag :inline-element) (eq tag :typed-element) (eq tag :ref-element))
              "element")
             ((or (eq tag :inline-attribute) (eq tag :typed-attribute) (eq tag :ref-attribute))
              "attr")
             ((or (eq tag :inline-type) (eq tag :named-type))
              "type"))
            "]")))

(defun xsd--qualify-name (frame unqualified)
  "Construct a qualified qname using target namespace of FRAME and UNQUALIFIED name.

Has no effect if UNQUALIFIED is already qualified."
  (let ((target-namespace (plist-get frame :xsd-target-namespace)))
    (cond
     ((not target-namespace) (concat "[unqualified]:" (if unqualified unqualified "")))
     ((and unqualified (string-match-p ":" unqualified)) unqualified)
     (t
      (let ((ns-qualifier (assoc (plist-get frame :xsd-target-namespace)
                                 (plist-get frame :xsd-xmlns-alist))))
        (unless ns-qualifier
          (error "Unable to find qualifier for target namespace"))
        (concat (when (cdr ns-qualifier) (concat (cdr ns-qualifier) ":")) unqualified))))))

(defun xsd--namespaceify-name (xmlns-alist qname)
  "Construct \"{namespace}:name\" from XMLNS-ALIST and QNAME."
  (let* ((qualifier-name (split-string qname ":"))
         (qualifier (if (equal (length qualifier-name) 2)
                        (car qualifier-name)
                      nil))
         (name (if (equal (length qualifier-name) 2)
                   (car (cdr qualifier-name))
                 (car qualifier-name)))
         (ns-qualifier (rassoc qualifier xmlns-alist))
         (ns (if (equal qualifier "[unqualified]")
                 qualifier
               (and ns-qualifier (car ns-qualifier)))))
    (unless ns
      (error (format "Constructing id for unknown qualifier %s" qualifier)))
    (concat "{" ns "}" name)))

(defun xsd--set-namespace-qualification (frame namespace qualification)
  "Set the namespace qualification in for NAMESPACE to QUALIFICATION in FRAME."
  (let ((current (plist-get frame :xsd-namespace-qualification)))
    (cl-pushnew `(,namespace . ,qualification) current :test #'equal)
    (plist-put frame :xsd-namespace-qualification current)))

(defun xsd--add-doc (frame item doc-tag)
  "Set the documentation ITEM with the tag DOC-TAG to FRAME."
  (let* ((parent-id (car (plist-get frame :xsd-parent)))
         (entities (plist-get frame :xsd-entities))
         (parent (and parent-id (lax-plist-get entities parent-id))))
    (unless parent
      (error "No open entity to add documentation to"))
    (setq parent (xsd-set parent doc-tag (cons item (xsd-get parent doc-tag ))))
    (setq entities (lax-plist-put entities parent-id parent))
    (plist-put frame :xsd-entities entities)))

(defun xsd--doc-add-appinfo (frame appinfo)
  "Add APPINFO to FRAME."
  (xsd--add-doc frame appinfo :xsd-appinfo))

(defun xsd--doc-add-documentation (frame doc)
  "Add DOC to FRAME."
  (xsd--add-doc frame doc :xsd-doc))

(defun xsd--pop-annotation (frame)
  "Pop an annotation target in FRAME.

Return (update-frame . annotations)."
  (let* ((all-annotation (plist-get frame :xsd-annotation))
         (annotation (pop all-annotation)))
    `(,(plist-put frame :xsd-annotation all-annotation)
      .
      ,annotation)))

(defun xsd--get-appinfo (annotation)
  "Get the appinfo part of ANNOTATION."
  (plist-get annotation :xsd-appinfo))

(defun xsd--get-documentation (annotation)
  "Get the documentation part of ANNOTATION."
  (plist-get annotation :xsd-doc))

(defun xsd--append-path (frame entry)
  "Append ENTRY to the path of FRAME."
  (plist-put frame :xsd-path (cons entry (plist-get frame :xsd-path))))

(defun xsd--pop-path (frame)
  "Removes the latest entry of the path of FRAME."
  (plist-put frame :xsd-path (cdr (plist-get frame :xsd-path))))

(defun xsd--push-parent (frame parent)
  "Add PARENT to FRAME."
  (plist-put frame :xsd-parent (cons parent (plist-get frame :xsd-parent))))

(defun xsd--pop-parent (frame)
  "Remove the latest parent from FRAME."
  (plist-put frame :xsd-parent (cdr (plist-get frame :xsd-parent))))

(defun xsd-get-type (frame type)
  "Get TYPE from FRAME if defined else nil."
  (lax-plist-get (plist-get frame :xsd-types) type))

(defun xsd-element-p (entity)
  "Non-nil if ENTITY is an element."
  (let ((tag (xsd-get entity :tag)))
    (or (eq tag :ref-element)
        (eq tag :inline-element)
        (eq tag :typed-element))))

(defun xsd-attribute-p (entity)
  "Non-nil if ENTITY is an attribute."
  (let ((tag (xsd-get entity :tag)))
    (or (eq tag :ref-attribute)
        (eq tag :inline-attribute)
        (eq tag :typed-attribute))))

(defun xsd-object-p (entity)
  "Non-nil if ENTITY is an object (attribute or element)."
  (or (xsd-element-p entity) (xsd-attribute-p entity)))

(defun xsd-root-p (entity)
  "Non-nil if ENTITY is a root entity."
  (eq (xsd-get entity :tag) :root))

(defun xsd-type-p (entity)
  "Non-nil if ENTITY is a type entity."
  (let ((tag (xsd-get entity :tag)))
    (or (eq tag :inline-type) (eq tag :named-type))))

(defun xsd--sub-path-p (first second)
  "Non-nil if FIRST is a sub-path of SECOND."
  (let ((res t))
    (while (and first second res)
      (setq res (equal (car first) (car second)))
      (setq first (cdr first))
      (setq second (cdr second)))
    (and res (not first))))

(defun xsd--substract-path (first second)
  "Remove FIRST from the start of SECOND."
  (while first
    (setq first (cdr first))
    (setq second (cdr second)))
  second)

(defun xsd--interpret-object (entity xmlns-alist ns-qualification-alist)
  "Interpret ENTITY with XMLNS-ALIST by updating its name.

A name is only qualified if required according to NS-QUALIFICATION-ALIST."
  (if (xsd-get entity :name)
      (let* ((ns-name (xsd-get entity :name))
             (ns (progn
                   (if (string-match "{\\([^}]*\\)}\\(.*\\)" ns-name)
                       (match-string 1 ns-name)
                     (error (format "Interpreting object (%s) without namespace" ns-name)))))
             (name (match-string 2 ns-name))
             (ns-qualification (when (not (eq ns-qualification-alist :inhibited))
                                 (let ((q (assoc ns ns-qualification-alist)))
                                   (if q (cdr q) q))))
             (qualifier (cond
                         ((equal ns "[unqualified]") nil)
                         ((and (not (eq ns-qualification-alist :inhibited))
                               (or (eq (xsd-get entity :tag) :inline-element)
                                   (eq (xsd-get entity :tag) :typed-element))
                               (not (plist-get ns-qualification :xsd-elem-qualified))) nil)
                         ((and (not (eq ns-qualification-alist :inhibited))
                               (or (eq (xsd-get entity :tag) :inline-attribute)
                                   (eq (xsd-get entity :tag) :typed-attribute))
                               (not (plist-get ns-qualification :xsd-elem-qualified))) nil)
                         (t (let ((ns-qualifier (assoc ns xmlns-alist)))
                              ;; If no qualifier can be found, then this is a node
                              ;; that is not allowed to be present (e.g. it could be
                              ;; an unqualified element or a type from a namespace that is not
                              ;; is not specified in the xml-file tried for the root element)
                              (if ns-qualifier
                                  (cdr ns-qualifier)
                                ns))))))
        (xsd-set (copy-tree entity) :name (concat (if qualifier (concat qualifier ":") "") name)))
    entity))
  

(defun xsd--follow-object-path (path node entities xmlns-alist ns-qualification-alist
                                     &optional not-root)
  "Follow PATH starting in NODE using availble ENTITIES and XMLNS-ALIST.

NS-QUALIFICATION-ALIST is describing what should be qualified in a namespace.
NOT-ROOT is an internal variable."
  (let ((interpreted-node (xsd--interpret-object
                           node xmlns-alist
                           (if not-root ns-qualification-alist :inhibited))))
    (cond
     ((xsd-object-p node)
      (cond
       ((not path)
        ;; If it's the root then the namespace must be present in xmlns-alist.
        ;; Otherwise it might be from a namespace in some imported schema not used
        ;; directly in the xml-file.
        (if (or not-root (assoc (xsd-get interpreted-node :namespace) xmlns-alist))
            `(,interpreted-node)
          '()))
       ((equal (car path) (xsd-get interpreted-node :name))
        (cond
         ((or (eq (xsd-get node :tag) :ref-element)
              (eq (xsd-get node :tag) :ref-attribute))
          (xsd--follow-object-path path (lax-plist-get entities (xsd-get node :ref)) entities xmlns-alist ns-qualification-alist t))
         ((or (eq (xsd-get node :tag) :inline-element)
              (eq (xsd-get node :tag) :inline-attribute)
              (eq (xsd-get node :tag) :typed-element)
              (eq (xsd-get node :tag) :typed-attribute))
          (let ((path (cdr path))
                result)
            (dolist (child (xsd-get node :children))
              (setq result (nconc result (xsd--follow-object-path path (lax-plist-get entities child) entities xmlns-alist ns-qualification-alist t))))
            result))
         (t (error (format "Following unknown tag %s" (symbol-name (xsd-get node :tag)))))))
       (t nil)))
     ((or (xsd-root-p node) (xsd-type-p node))
      (let (result)
        (dolist (child (xsd-get node :children))
          (setq result (nconc result (xsd--follow-object-path path (lax-plist-get entities child) entities xmlns-alist ns-qualification-alist not-root))))
        result)))))

(defun xsd-get-object (frame path)
  "Get all objects in FRAME that is reachable as a PATH from root.

For example, (xsd-get-object some-frame '(\"schema\" \"element\") is equal
to getting all objects (attributes and elements) that are children of an object
in xpath /schema/element.
PATH is a list of strings that forms the path.
If PATH is :all then all objects in FRAME is returned."
  (let ((result '())
        (entities (plist-get frame :xsd-entities))
        (xmlns-alist (plist-get frame :xsd-xmlns-alist))
        (ns-qualification-alist (plist-get frame :xsd-namespace-qualification))
        xsd-id entity)
    (cond
     ((eq path :all)
      (while entities
        (setq xsd-id (pop entities))
        (setq entity (pop entities))
        (when (or (xsd-attribute-p entity)
                  (xsd-element-p entity))
          (add-to-list 'result (xsd--interpret-object entity xmlns-alist :inhibited)))))
     (t
      (setq result (xsd--follow-object-path path (lax-plist-get entities xsd--root-xsd-id) entities xmlns-alist ns-qualification-alist))))
    result))

(defun xsd--add-child (frame child-id parent-id)
  "Add CHILD-ID to as a child of PARENT-ID in FRAME."
  (let* ((entities (plist-get frame :xsd-entities))
         (parent (lax-plist-get entities parent-id)))
    (setq parent (plist-put parent :children (cons child-id (plist-get parent :children))))
    (setq entities (lax-plist-put entities parent-id parent))
    (plist-put frame :xsd-entities entities)))

(defun xsd--add-child-top-parent (frame child-id)
  "Add CHILD-ID as child to the top of parents in FRAME."
  (xsd--add-child frame child-id (car (plist-get frame :xsd-parent))))

(defun xsd--insert-raw-entity (frame item)
  "Insert ITEM into FRAME without adding anything."
  (let ((entities (plist-get frame :xsd-entities)))
    (plist-put frame :xsd-entities
               (lax-plist-put entities (xsd-get item :xsd-id) item))))

(defun xsd--insert-entity-in-namespace (frame item)
  "Insert ITEM into FRAME using current namespace."
  (xsd--insert-raw-entity frame (plist-put item :namespace (plist-get frame :xsd-target-namespace))))

(defun xsd-merge-frames (frame other)
  "Merge FRAME and OTHER by mergin entities from OTHER into FRAME.

Both frames may be modified during the merge."
  (if frame
      (let ((other-entities (plist-get other :xsd-entities))
            entity-xsd-id entity)
        (while other-entities
          (setq entity-xsd-id (pop other-entities))
          (setq entity (pop other-entities))
          (if (equal entity-xsd-id xsd--root-xsd-id)
              ;; merge roots
              (let* ((root (lax-plist-get (plist-get frame :xsd-entities) xsd--root-xsd-id))
                     (children (plist-get root :children)))
                (unless root
                  (plist-put root :xsd-id xsd--root-xsd-id))
                (setq root (plist-put root :children (append (plist-get entity :children) children)))
                (setq frame (xsd--insert-raw-entity frame root)))
            ;; simply add the new entities
            (setq frame (xsd--insert-raw-entity frame entity))
            (dolist (ns-qualification (plist-get other :xsd-namespace-qualification))
              (setq frame (xsd--set-namespace-qualification frame (car ns-qualification) (cdr ns-qualification))))))
        frame)
    other))

(defsubst xsd--match-tag-p (node tag)
  "Checks whether TAG matches the tag of the NODE."
  (equal (symbol-name (dom-tag node)) tag))

(defun xsd--visit-schema (frame node)
  "Compile a schema NODE under FRAME."
  (let ((old-xmlns-alist (plist-get frame :xsd-xmlns-alist)))
    (setq frame (xsd--set-path frame '((:root . nil))))
    (setq frame (xsd--push-parent frame xsd--root-xsd-id))
    (setq frame (xsd--insert-raw-entity frame `(:tag :root :xsd-id ,xsd--root-xsd-id)))
    (setq frame (plist-put frame :xsd-target-namespace (dom-attr node 'targetNamespace)))
    (setq
     frame
     (plist-put frame :xsd-xmlns-alist
                (let (xmlns-alist)
                  (dolist (attr (dom-attributes node))
                    (when (string-match-p "\\`xmlns\\(?::\\w+\\)?\\'" (symbol-name (car attr)))
                      (add-to-list 'xmlns-alist `(,(cdr attr)
                                                  .
                                                  ,(car (cdr (split-string (symbol-name (car attr)) ":")))))))
                  xmlns-alist)))
    (when (and (not (plist-get frame :xsd-target-namespace))
               (rassoc nil (plist-get frame :xsd-xmlns-alist)))
      (error "XSD schema is both unqualified and has unqualified namespace"))
    (when (not (plist-get frame :xsd-target-namespace))
      (setq frame (plist-put frame :xsd-target-namespace "[unqualified]"))
      (setq
       frame
       (plist-put frame :xsd-xmlns-alist
                  (cons '("[unqualified]" . nil) (plist-get frame :xsd-xmlns-alist)))))
    (let ((req-attr (equal (dom-attr node 'attributeFormDefault) "qualified"))
          (req-elem (equal (dom-attr node 'elementFormDefault) "qualified")))
      (setq frame (xsd--set-namespace-qualification
                   frame (plist-get frame :xsd-target-namespace)
                   `(:xsd-elem-qualified ,req-elem :xsd-attr-qualified ,req-attr))))
    (setq frame (xsd--visit-children frame node))
    (setq frame (xsd--pop-path frame))
    (setq frame (xsd--pop-parent frame))
    (plist-put frame :xsd-xmlns-alist old-xmlns-alist)))

(defun xsd--visit-complex-type (frame node)
  "Compile a complex type NODE under FRAME."
  (let* ((qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                        (xsd--qualify-name frame (dom-attr node 'name))))
         (tag (if qname :named-type :inline-type))
         (xsd-id (xsd--construct-id (plist-get frame :xsd-path) tag qname)))
    (setq frame (xsd--insert-entity-in-namespace frame `(:xsd-id ,xsd-id :tag ,tag :name ,qname)))
    (unless (eq (car (car (plist-get frame :xsd-path))) :root)
      (setq frame (xsd--add-child-top-parent frame xsd-id)))
    (setq frame (xsd--append-path frame `(,tag . ,qname)))
    (setq frame (xsd--push-parent frame xsd-id))
    (setq frame (xsd--visit-children frame node))
    (setq frame (xsd--pop-parent frame))
    (xsd--pop-path frame)))

(defun xsd--visit-element (frame node)
  "Compile an element NODE under FRAME."
  (let (qname xsd-id tag ref children)
    (cond
     ((dom-attr node 'ref)
      (setq tag :ref-element)
      (setq qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                          (dom-attr node 'ref)))
      (setq ref (xsd--construct-id '((:root . nil)) :ref-element qname)))
     ((dom-attr node 'type)
      (setq tag :typed-element)
      (setq qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                          (xsd--qualify-name frame (dom-attr node 'name))))
      (setq children `(,(xsd--construct-id
                         '((:root . nil))
                         :named-type (xsd--namespaceify-name
                                      (plist-get frame :xsd-xmlns-alist)
                                      (dom-attr node 'type)))))
      (unless qname
        (error "Typed element without name")))
     (t
      (setq tag :inline-element)
      (setq qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                          (xsd--qualify-name frame (dom-attr node 'name))))
      (unless qname
        (error "Inline element without name"))))
    (setq xsd-id (xsd--construct-id (plist-get frame :xsd-path) tag qname))
    (setq frame (xsd--insert-entity-in-namespace
                 frame
                 `(:tag ,tag :xsd-id ,xsd-id :name ,qname
                   :children ,children :ref ,ref)))
    (setq frame (xsd--add-child-top-parent frame xsd-id))
    (setq frame (xsd--append-path frame `(,tag . ,qname)))
    (setq frame (xsd--push-parent frame xsd-id))
    (setq frame (xsd--visit-children frame node))
    (setq frame (xsd--pop-parent frame))
    (xsd--pop-path frame)))

(defun xsd--visit-attribute (frame node)
  "Compile an attribute NODE under FRAME."
  (let ((usage (dom-attr node 'use))
        qname xsd-id tag)
    (setq usage (cond
                 ((equal usage "required") :required)
                 ((equal usage "prohibited") :prohibited)
                 (t :optional)))
    (cond
     ((dom-attr node 'ref)
      (setq qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                          (dom-attr node 'ref)))
      (setq tag :ref-attribute))
     ((dom-attr node 'type)
      (setq tag :typed-attribute)
      (setq qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                          (xsd--qualify-name frame (dom-attr node 'name))))
      (unless qname
        (error "Typed attribute without name")))
     (t
      (setq tag :inline-attribute)
      (setq qname (xsd--namespaceify-name (plist-get frame :xsd-xmlns-alist)
                                          (xsd--qualify-name frame (dom-attr node 'name))))
      (unless qname
        (error "Inline attribute without name"))))
    (setq xsd-id (xsd--construct-id (plist-get frame :xsd-path) tag qname))
    (setq frame (xsd--insert-entity-in-namespace
                 frame
                 `(:tag ,tag :xsd-id ,xsd-id :name ,qname :usage ,usage)))
    (setq frame (xsd--add-child-top-parent frame xsd-id))
    (setq frame (xsd--append-path frame `(,tag . ,qname)))
    (setq frame (xsd--push-parent frame xsd-id))
    (setq frame (xsd--visit-children frame node))
    (setq frame (xsd--pop-parent frame))
    (xsd--pop-path frame)))

(defalias 'xsd--visit-annotation 'xsd--visit-children
  "Visit an annotation NODE under FRAME.")

(defun xsd--visit-appinfo (frame node)
  "Visit an appinfo NODE under FRAME."
  (xsd--doc-add-appinfo frame (dom-text node)))

(defun xsd--visit-documentation (frame node)
  "Visit a documentation NODE under FRAME."
  (xsd--doc-add-documentation frame (dom-text node)))

(defun xsd--visit-import (frame node)
  "Visit an import NODE under FRAME."
  (let ((location (dom-attr node 'schemaLocation)))
    (if location
        (xsd-merge-frames frame (xsd-compile-uri (xsd--as-absolute-uri frame location)))
      frame)))

(defun xsd--visit-include (frame node)
  "Visit an include NODE under FRAME."
  (let ((uri (dom-attr node 'schemaLocation)))
    (unless uri
      (error "Missing uri in include element"))
    ;; To only allowed children of include elements are annotations
    ;; and they are safe to ignore for the purpose of completion.
    ;; Hence, ignore them.
    (xsd-merge-frames frame (xsd-compile-uri (xsd--as-absolute-uri frame uri)))))

(defun xsd--visit-children (frame node)
  "Visit all children of NODE under FRAME."
  (dolist (child (dom-children node))
    (setq frame (xsd--visit frame child)))
  frame)

(defun xsd--visit (frame node)
  "Visit NODE for compilation in the current FRAME."
  (let ((schema-qualifier (plist-get frame :xsd-schema-qualifier)))
    (cond
     ;; Unstructured nodes (i.e. text nodes) are safe to ignore since all data in xsd schema are structured data
     ((stringp node) frame)
     ((xsd--match-tag-p node (concat schema-qualifier "annotation"))
      (xsd--visit-annotation frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "appinfo"))
      (xsd--visit-appinfo frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "attribute"))
      (xsd--visit-attribute frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "complexType"))
      (xsd--visit-complex-type frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "documentation"))
      (xsd--visit-documentation frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "element"))
      (xsd--visit-element frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "import"))
      (xsd--visit-import frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "include"))
      (xsd--visit-include frame node))
     ((xsd--match-tag-p node (concat schema-qualifier "schema"))
      (xsd--visit-schema frame node))
     (t (xsd--visit-children frame node)))))

(defun xsd--set-frame-namespace (frame namespace)
  "Set all elements in FRAME to use NAMESPACE."
  (let ((entities (reverse (plist-get frame :xsd-entities)))
        entity xsd-id
        new-entities)
    (while entities
      (setq entity (pop entities))
      (when (xsd-element-p entity)
        (setq entity (plist-put entity :name (concat namespace ":" (plist-get entity :name)))))
      (setq xsd-id (pop entities))
      (setq new-entities (lax-plist-put new-entities xsd-id entity)))
    (plist-put frame :xsd-entities new-entities)))

(defun xsd--get-xml-schema-qualifier (xml)
  "Get the qualifier for http://www.w3.org/2001/XMLSchema-instance in XML."
  (let ((attributes (dom-attributes xml))
        qualifier)
    (while (and (not qualifier) attributes)
      (let ((attr (car attributes)))
        (when (equal (cdr attr) "http://www.w3.org/2001/XMLSchema")
          (if (string-match-p ":" (symbol-name (car attr)))
              (setq qualifier (concat (car (cdr (split-string (symbol-name (car attr)) ":"))) ":"))
            (setq qualifier ""))))
      (setq attributes (cdr attributes)))
    qualifier))

(defun xsd-interpret-compilation-frame (frame namespace-qualifier-alist)
  "Interpret a FRAME using the NAMESPACE-QUALIFIER-ALIST."
  (plist-put frame :xsd-xmlns-alist namespace-qualifier-alist))

(defun xsd-print-compilation-frame (frame)
  "Prints FRAME to temporary buffer."
  (with-output-to-temp-buffer "*XSD-comp*"
    (princ "Path: ")
    (princ (xsd--human-viewable-path frame))
    (terpri)
    (princ "Entities: ")
    (terpri)
    (xsd--print-entities (plist-get frame :xsd-entities))))

(defun xsd-compile-buffer (&optional buffer uri)
  "Compile the xsd schema in BUFFER.

URI is URI for the schema."
  (interactive)
  (unless buffer
    (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (let ((xml (xml-parse-region (point-min) (point-max))))
      (xsd--visit (xsd--set-schema-uri
                   (plist-put (xsd--empty-frame) :xsd-schema-qualifier
                              (xsd--get-xml-schema-qualifier xml))
                   uri)
                  xml))))

(defun xsd-compile-uri (uri)
  "Compile the xsd-file at URI."
  (with-current-buffer (xsd-uri-fetch uri)
    (xsd-compile-buffer (current-buffer) uri)))

(defun xsd-compile-file (path)
  "Compile the xsd-file at PATH.

Most likely slightly faster than xsd-compile-uri.
However, only recommended to use when it is known that it's a file without
checking."
  (let ((xml (xml-parse-file path)))
    (xsd--visit (xsd--set-schema-uri
                 (plist-put (xsd--empty-frame) :xsd-schema-qualifier
                            (xsd--get-xml-schema-qualifier xml))
                 (concat "file://" (file-truename path)))
                xml)))

(provide 'xsd)
;;; xsd.el ends here
