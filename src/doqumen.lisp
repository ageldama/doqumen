(defpackage #:doqumen
  (:use #:cl #:iterate)
  (:import-from #:rutils #:-> #:% #:appendf)
  (:export :build-doc))

(in-package :doqumen)




(defun directory-pn (pn)
  (if (uiop:directory-pathname-p pn) pn
      (uiop:pathname-directory-pathname pn)))

(defun system-definition-dir (system-name)
  (-> system-name
      asdf:find-system
      asdf:system-source-file
      directory-pn))

(defun merge-pn-with-asdf-system-path (pn system-name)
  (-> system-name
      system-definition-dir
      (uiop:merge-pathnames* pn %)))



(defun copy-file-into-stream (in-file out-stream)
  (with-open-file (in-stream in-file :direction :input)
    (uiop:copy-stream-to-stream in-stream out-stream)))





(defvar *system-name* nil)

(defvar *seed-symbol* nil)
(defvar *seed-prop-name* nil)

(defvar *seed-plist* nil)


(defun seed-plist ()
  (assert *seed-symbol* (*seed-symbol*))
  (assert *seed-prop-name* (*seed-prop-name*))
  (let ((seed-plist (get *seed-symbol* *seed-prop-name*)))
    seed-plist))



(defvar *output-pn* nil)
(defvar *out-stream* nil)

(defvar *docparser-index* nil)

(defvar *api-refs* nil)

(defvar *api-ref-anchor-prefix* "API-")



;; TODO api-refs: printer function
;; TODO api-refs->toc


(defmethod type-keyword (node) :unknown)

(defmethod type-keyword ((node docparser:cffi-function)) :cffi-function)

(defmethod type-keyword ((node docparser:cffi-type)) :cffi-type)

(defmethod type-keyword ((node docparser:cffi-slot)) :cffi-slot)

(defmethod type-keyword ((node docparser:cffi-struct)) :cffi-struct)

(defmethod type-keyword ((node docparser:cffi-union)) :cffi-union)

(defmethod type-keyword ((node docparser:cffi-enum)) :cffi-enum)

(defmethod type-keyword ((node docparser:cffi-bitfield)) :cffi-bitfield)

(defmethod type-keyword ((node docparser:function-node)) :function)

(defmethod type-keyword ((node docparser:macro-node)) :macro)

(defmethod type-keyword ((node docparser:generic-function-node)) :generic-function)

(defmethod type-keyword ((node docparser:method-node)) :method)

(defmethod type-keyword ((node docparser:variable-node)) :variable)

(defmethod type-keyword ((node docparser:struct-slot-node)) :struct-slot)

(defmethod type-keyword ((node docparser:struct-node)) :struct)

(defmethod type-keyword ((node docparser:condition-node)) :condition)

(defmethod type-keyword ((node docparser:class-slot-node)) :class)

(defmethod type-keyword ((node docparser:class-node)) :class)

(defmethod type-keyword ((node docparser:type-node)) :type)




(defmethod node-info-list (node) (list))


(defmethod node-info-list ((node docparser:operator-node))
  (list    :docstring (docparser:node-docstring node)))


(defmethod node-info-list ((node docparser:cffi-function))
  (list
   :cffi-name
   (docparser:cffi-function-foreign-name node)
   :lambda-list
   (docparser:operator-lambda-list node)
   :setfp
   (docparser:operator-setf-p node)
   :cffi-return-type
   (docparser:cffi-function-return-type node)))

(defmethod node-info-list ((node docparser:cffi-type))
  (list
   :docstring (docparser:node-docstring node)
   :base-type
   (docparser:cffi-type-base-type node)))

(defmethod node-info-list ((node docparser:cffi-slot))
  (list
   :name
   (docparser:node-name node)
   :type
   (docparser:cffi-slot-type node)))

(defmethod node-info-list ((node docparser:cffi-struct))
  (list
   :docstring (docparser:node-docstring node)
   :slots (loop for slot in (docparser:cffi-struct-slots node)
                collect (node-info-list slot))))

(defmethod node-info-list ((node docparser:cffi-union))
  ;; FIXME: ->nil
  (list
   :docstring (docparser:node-docstring node)
   :variants (docparser:cffi-union-variants node)))

(defmethod node-info-list ((node docparser:cffi-enum))
  ;; FIXME: ->nil
  (list
   :docstring (docparser:node-docstring node)
   :variants (docparser:cffi-enum-variants node)))

(defmethod node-info-list ((node docparser:cffi-bitfield))
  (list
   :docstring (docparser:node-docstring node)
   :masks (docparser:cffi-bitfield-masks node)))

(defmethod node-info-list ((node docparser:function-node))
  (list
   :lambda-list
   (docparser:operator-lambda-list node)
   :setfp
   (docparser:operator-setf-p node)))

(defmethod node-info-list ((node docparser:macro-node))
  (list
   :lambda-list
   (docparser:operator-lambda-list node)
   :setfp
   (docparser:operator-setf-p node)))

(defmethod node-info-list ((node docparser:generic-function-node))
  (list
   :lambda-list
   (docparser:operator-lambda-list node)
   :setfp
   (docparser:operator-setf-p node)))

(defmethod node-info-list ((node docparser:method-node))
  (list
   :lambda-list
   (docparser:operator-lambda-list node))
  :setfp
  (docparser:operator-setf-p node)
   ;; FIXME: ->nil
   :qualifiers
  (docparser:method-qualifiers node))

(defmethod node-info-list ((node docparser:variable-node))
  (list
   :docstring (docparser:node-docstring node)
   :variable-initial-value
   (docparser:variable-initial-value node)))

(defmethod node-info-list ((node docparser:struct-slot-node))
  (list
   :name
   (docparser:node-name node)
   :type
   (docparser:struct-slot-type node)
   :accessor
   (docparser:struct-slot-accessor node)
   :read-only
   (docparser:struct-slot-read-only node)
   :initform
   (docparser:slot-initform node)))

(defmethod node-info-list ((node docparser:struct-node))
  (list
   :docstring (docparser:node-docstring node)
   :slots (loop for slot in (docparser:record-slots node)
                collect (node-info-list slot))
   :conc-name (docparser:struct-node-conc-name node)
   :constructor (docparser:struct-node-constructor node)
   :copier (docparser:struct-node-copier node)
   :include-name (docparser:struct-node-include-name node)
   :include-slots (docparser:struct-node-include-slots node)
   :initial-offset (docparser:struct-node-initial-offset node)
   :named (docparser:struct-node-named node)
   :predicate (docparser:struct-node-predicate node)
   :print-function (docparser:struct-node-print-function node)
   :print-object (docparser:struct-node-print-object node)
   :type (docparser:struct-node-type node)))

(defmethod node-info-list ((node docparser:class-slot-node))
  (list
   :name (docparser:node-name node)
   :docstring (docparser:node-docstring node)
   :accessors (docparser:slot-accessors node)
   :readers   (docparser:slot-readers node)
   :writers   (docparser:slot-writers node)
   :type      (docparser:slot-type    node)
   :initarg   (docparser:slot-initarg node)
   :initform  (docparser:slot-initform node)
   :allocation (docparser:slot-allocation node)))




(defmethod node-info-list ((node docparser:class-node))
  (list
   :docstring (docparser:node-docstring node)
   :superclasses (docparser:class-node-superclasses node)
   :metaclass (docparser:class-node-metaclass node)
   :default-initargs (docparser:class-node-default-initargs node)
   :slots (loop for slot in (docparser:record-slots node)
                collect (node-info-list slot))))

(defmethod node-info-list ((node docparser:type-node))
  (list
   :lambda-list
   (docparser:operator-lambda-list node)
   :setfp
   (docparser:operator-setf-p node)))






(defun gather-api-docs ()
  (let ((results '()))
    (docparser:do-packages (pkg *docparser-index*)
      (let ((pkg-name (docparser:package-index-name pkg))
            (pkg-doc  (docparser:package-index-docstring pkg))
            (pkg-symbols '()))
        ;;
        (docparser:do-nodes (node pkg)
          (appendf pkg-symbols
                   (list (list :name (docparser:node-name node)
                               :type (type-keyword node)
                               :docstring (docparser:node-docstring node)
                               :info (node-info-list node)))))
        ;;
        (appendf results
                 (list (list :pkg-name pkg-name
                             :docstring pkg-doc
                             :symbols   pkg-symbols)))))
    ;;
    results))



(defparameter *anchor-uri-encode-func*  #'quri:url-encode)


(defun print-html-anchor (text anchor-uri)
  (format *out-stream* "<a name=\"~a\">~a</a>~%"
          (funcall *anchor-uri-encode-func* anchor-uri)
          (cl-who:escape-string text)))


(defparameter *print-anchor-func* #'print-html-anchor)



(defvar *toc* nil)




(defun print-api-ref ()
  (log:info "PRINT API-REF ...")
  ;; TODO
  ;;(print *api-refs* *out-stream*)
  )



(defun extract-first-heading-from-markdown-file (pn)
  (iter (for line in-file
             pn
             using #'read-line)
    (setf line (str:trim line))
    (multiple-value-bind (match-string reg-strings)
        (cl-ppcre:scan-to-strings "^(#+)\\s*(.*)" line)
      (when match-string
        (return (aref reg-strings 1))))))



(defvar *toc-heading* "# Table of Contents")

(defvar *toc-title* "Table of Contents")

(defvar *toc-anchor* "TOC")


(defvar *api-ref-heading* "# APIs")

(defvar *api-ref-title* "APIs")

(defvar *api-ref-anchor* "API-REF")

(defparameter *section-file-title-func*
  #'extract-first-heading-from-markdown-file)


(defun print-toc-as-markdown
    (toc &key (level 1))
  ;;
  (when (eq 1 level)
    (funcall *print-anchor-func* "" *toc-anchor*)
    (format *out-stream* "~A~%~%" *toc-heading*))
  ;;
  (dolist (e toc)
    (format *out-stream* "~v@{~A~:*~}" level "   ")
    (let ((text (getf e :text))
          (anchor (getf e :anchor)))
      (format *out-stream* "1. [~A](#~A)~%"
              text
              (funcall *anchor-uri-encode-func* anchor)))
    ;;
    (when (getf e :children)
      (print-toc-as-markdown (getf e :children)
                             :level (1+ level))))
  ;;
  (when (eq 1 level)
    (format *out-stream* "~%")))



(defparameter *print-toc-func* #'print-toc-as-markdown)


(defun print-toc ()
  (log:info "PRINT TOC ...")
  (funcall *print-toc-func* *toc*))




(defun root-sections ()
    (getf *seed-plist* :sections))


(defun find+apply (prefix sym &rest args)
  (apply (-> sym
             string
             (format nil "~a~a" prefix %)
             string-upcase
             intern)
         args))



(defmacro toc-appendf (place &key text anchor children)
  (let ((%lst (gensym)))
    `(let ((,%lst '()))
       (appendf ,%lst (list :text ,text :anchor ,anchor))
       (when ,children
         (appendf ,%lst (list :children ,children)))
       (appendf ,place (list ,%lst))
       ;;
       ,place)))


(defun build-toc
    (&key sections)
  (let ((toc nil)
        (sections* (or sections (root-sections))))
    (dolist (section sections*)
      (cond
        ((listp section)
         (let* ((subl-l (build-toc :sections (list (first section))))
                (subl-r (build-toc :sections (rest section))))
           (toc-appendf toc
                        :text (getf (first subl-l) :text)
                        :anchor (getf (first subl-l) :anchor)
                        :children subl-r)))
        ((eq section :toc)
         (toc-appendf toc :text *toc-title*
                          :anchor *toc-anchor*))
        ((eq section :api-ref)
         (toc-appendf toc :text *api-ref-title*
                          :anchor *api-ref-anchor*))
        ((pathnamep section)
         (toc-appendf toc :text (or (funcall *section-file-title-func* section)
                                    (format nil "~A" section))
                          :anchor (format nil "~A" section)))
        (t
         (toc-appendf toc :text section :anchor section))))
    toc))





(defun print-sections
    (&key sections)
  (let ((sections* (or sections (root-sections))))
    (dolist (section sections*)
      (cond
        ((keywordp section)
          (case section
            (:toc (print-toc))
            (:api-ref (print-api-ref))
            ;; They told me I could be anything so I...:
            (t (find+apply "print-" section))))
        ((listp section)
         (print-sections :sections section))
        (t ;; else: just a section
          (let ((in-pn
                  (merge-pn-with-asdf-system-path section *system-name*)))
            (log:debug "COPYING FROM: ~a" in-pn)
            (funcall *print-anchor-func* "" (format nil "~A" section))
            (copy-file-into-stream in-pn *out-stream*)))))))


(defun init-logger ()
  (log:config :debug))



(defun build-doc
    (system-name
     &key
       seed-symbol
       (seed-prop-name :doqumen)
       (output-file #p"docs/index.md"))
  "Build it!"
  ;;
  (init-logger)
  ;;
  (log:info "LOADING SYSTEM: ~a" system-name)
  (asdf:find-system system-name)
  ;;
  (let* ((*system-name* system-name)
         (*output-pn* (merge-pn-with-asdf-system-path output-file
                                                      system-name))
         (*seed-symbol* (or seed-symbol system-name))
         (*seed-prop-name*    seed-prop-name)
         (*seed-plist* (seed-plist)))
    ;;
    (log:expr *output-pn*)
    (log:expr *seed-symbol*)
    (log:expr *seed-prop-name*)
    (log:expr *seed-plist*)
    (assert *seed-plist* (*seed-plist*))
    ;;
    (log:info "DOC-PARSING: ~a ..." *system-name*)
    (let ((*docparser-index* (docparser:parse *system-name*)))
      (log:info "GATHERING API-DOCS ...")
      (let ((*api-refs* (gather-api-docs)))
        (log:info "BUILDING TOC ...")
        (let ((*toc* (build-toc)))
          ;;
          (log:info "ENSURE-DIR: ~a ..." *output-pn*)
          (ensure-directories-exist *output-pn*)
          (log:info "WRITING: ~a ..." *output-pn*)
          (uiop:with-output-file (*out-stream*
                                  *output-pn*
                                  :if-exists :supersede)
            (print-sections)))))))






;;; EXAMPLE: ... how it does "doqumen" itself:
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Same keyword with name of ASDF-system:
  (setf (get :doqumen :doqumen)
        `(:sections (
                     ,#p"src/01-title.md"
                     :toc
                     (
                      ;; 1st item in a list is heading.
                      ,#p"src/02-intro.md"
                      ;; ...rests are subheadings:
                      ,#p"src/02-a-rationale.md"
                      )
                     :api-ref
                     ))))





;;; TODO defpkg:export
