(defpackage #:doqumen
  (:use #:cl #:iterate)
  (:import-from #:rutils #:-> #:% #:appendf)
  (:import-from #:rutils.anaphora #:awhen #:it)
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
   (docparser:operator-lambda-list node)
   :setfp
   (docparser:operator-setf-p node)
   :qualifiers
   (docparser:method-qualifiers node)
   ))

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




(defun api-ref-code-string-in-markdown (string)
  (format nil "`~A`" string))


(defparameter *api-ref-code-string-func* #'api-ref-code-string-in-markdown)

(defun api-ref-code-string (string)
  (funcall *api-ref-code-string-func* string))




(defun api-refs-sort-toc-symbols-text-lexico (symbs)
  (sort symbs (lambda (a b)
                (string-lessp
                 (getf a :text)
                 (getf b :text)))))

(defun api-refs-sort-toc-packages-text-lexico (pkgs)
  (sort pkgs (lambda (a b)
                (string-lessp
                 (getf a :text)
                 (getf b :text)))))



(defparameter *api-refs-sort-toc-symbols-func*
  #'api-refs-sort-toc-symbols-text-lexico)

(defparameter *api-refs-sort-toc-packages-func*
  #'api-refs-sort-toc-packages-text-lexico)


(defun api-refs-sort-toc-symbols (symbs)
  (funcall *api-refs-sort-toc-symbols-func* symbs))

(defun api-refs-sort-toc-packages (symbs)
  (funcall *api-refs-sort-toc-packages-func* symbs))




(defun gather-api-refs ()
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



(defun slugify (string)
  (let* ((lowercase (string-downcase string))
         (cleaned (cl-ppcre:regex-replace-all "[^a-z0-9\\s-]" lowercase ""))
         (trimmed (string-trim " " cleaned))
         (slugged (cl-ppcre:regex-replace-all "[\\s-]+" trimmed "-")))
    slugged))

(defun bytes-to-hex (bytes)
  (with-output-to-string (out)
    (loop for byte across bytes
          do (format out "~2,'0x" byte))))

(defun slugify+md5hex (string)
  (format nil "~a_~a"
          (slugify string)
          (-> string
              md5:md5sum-string
              bytes-to-hex)))


(defparameter *anchor-uri-encode-func*  #'slugify+md5hex)


(defun print-html-anchor (text anchor-uri)
  (format *out-stream* "<a name=\"~a\">~a</a>~%"
          (funcall *anchor-uri-encode-func* anchor-uri)
          (cl-who:escape-string text)))


(defparameter *print-anchor-func* #'print-html-anchor)



(defvar *toc* nil)



(defgeneric print-api-ref-body-as-markdown (type api-ref out-stream))

(defmethod print-api-ref-body-as-markdown ((type t) api-ref out-stream)
  (awhen (getf api-ref :docstring)
    (format out-stream "~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-function)) api-ref out-stream)
  (format out-stream "- CFFI NAME: `~W`~%"
          (getf (getf api-ref :info) :cffi-name))
  (format out-stream "- CFFI RETURN-TYPE: `~W`~%"
          (getf (getf api-ref :info) :cffi-return-type))
  (format out-stream "- LAMBDA LIST: `~W`~%"
          (getf (getf api-ref :info) :lambda-list))
  (format out-stream "- SETF? `~W`~%"
          (getf (getf api-ref :info) :setfp))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-type))
                                           api-ref out-stream)
  (format out-stream "- BASE-TYPE: `~W`~%"
          (getf (getf api-ref :info) :base-type))
  (awhen (getf (getf api-ref :info) :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-slot))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "   - SLOT `~A` / TYPE: `~A`~%"
            (getf info :name) (getf info :type)))
  (awhen (getf api-ref :docstring)
    (format out-stream "      - ~A~%" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-struct))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "- SLOTS:~%")
    (dolist (slot (getf info :slots))
      (print-api-ref-body-as-markdown :cffi-slot slot out-stream)))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-union))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "- VARIANTS: `~W`~%" (getf info :variants)))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-enum))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "- VARIANTS: `~W`~%" (getf info :variants)))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :cffi-bitfield))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "- MASKS: `~W`~%" (getf info :masks)))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :function)) api-ref out-stream)
  (format out-stream "- LAMBDA LIST: `~W`~%"
          (getf (getf api-ref :info) :lambda-list))
  (format out-stream "- SETF? `~W`~%"
          (getf (getf api-ref :info) :setfp))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :macro)) api-ref out-stream)
  (format out-stream "- LAMBDA LIST: `~W`~%"
          (getf (getf api-ref :info) :lambda-list))
  (format out-stream "- SETF? `~W`~%"
          (getf (getf api-ref :info) :setfp))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :type)) api-ref out-stream)
  (format out-stream "- LAMBDA LIST: `~W`~%"
          (getf (getf api-ref :info) :lambda-list))
  (format out-stream "- SETF? `~W`~%"
          (getf (getf api-ref :info) :setfp))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :generic-function))
                                           api-ref out-stream)
  (format out-stream "- LAMBDA LIST: `~W`~%"
          (getf (getf api-ref :info) :lambda-list))
  (format out-stream "- SETF? `~W`~%"
          (getf (getf api-ref :info) :setfp))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :method))
                                           api-ref out-stream)
  (format out-stream "- LAMBDA LIST: `~W`~%"
          (getf (getf api-ref :info) :lambda-list))
  (format out-stream "- SETF? `~W`~%"
          (getf (getf api-ref :info) :setfp))
  (format out-stream "- QUALIFIERS: `~W`~%"
          (getf (getf api-ref :info) :qualifiers))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :variable))
                                           api-ref out-stream)
  (format out-stream "- INITIAL-VALUE: `~W`~%"
          (getf (getf api-ref :info) :variable-initial-value))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :struct))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "- SLOTS:~%")
    (dolist (slot (getf info :slots))
      (print-api-ref-body-as-markdown :struct-slot slot out-stream))
    (format out-stream "- CONC-NAME: `~W`~%" (getf info :conc-name))
    (format out-stream "- CONSTRUCTOR: `~W`~%" (getf info :constructor))
    (format out-stream "- COPIER: `~W`~%" (getf info :copier))
    (format out-stream "- INCLUDE-NAME: `~W`~%" (getf info :include-name))
    (format out-stream "- INCLUDE-SLOTS: `~W`~%" (getf info :include-slots))
    (format out-stream "- INITIAL-OFFSET: `~W`~%" (getf info :initial-offset))
    (format out-stream "- NAMED: `~W`~%" (getf info :named))
    (format out-stream "- PREDICATE: `~W`~%" (getf info :predicate))
    (format out-stream "- PRINT-FUNCTION: `~W`~%" (getf info :print-function))
    (format out-stream "- PRINT-OBJECT: `~W`~%" (getf info :print-object))
    (format out-stream "- TYPE: `~W`~%" (getf info :type)))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :struct-slot))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "   - SLOT `~A` / TYPE: `~A` / READ-ONLY? `~W`~%"
            (getf info :name) (getf info :type) (getf info :read-only))
    (format out-stream "      - INITFORM: `~W`~%" (getf info :initform))
    (format out-stream "      - ACCESSOR: `~W`~%" (getf info :ACCESSOR))))

(defmethod print-api-ref-body-as-markdown ((type (eql :class))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "- SLOTS:~%")
    (dolist (slot (getf info :slots))
      (print-api-ref-body-as-markdown :class-slot slot out-stream))
    (format out-stream "- SUPERCLASSES: `~W`~%" (getf info :superclasses))
    (format out-stream "- METACLASS: `~W`~%" (getf info :metaclass))
    (format out-stream "- DEFAULT-INITARGS: `~W`~%" (getf info :default-initargs))
    (format out-stream "- TYPE: `~W`~%" (getf info :type)))
  (awhen (getf api-ref :docstring)
    (format out-stream "~%~A" it)))

(defmethod print-api-ref-body-as-markdown ((type (eql :class-slot))
                                           api-ref out-stream)
  (let ((info (getf api-ref :info)))
    (format out-stream "   - SLOT `~A` / TYPE: `~A`~%"
            (getf info :name) (getf info :type))
    (format out-stream "      - ALLOCATION: `~W`~%" (getf info :allocation))
    (format out-stream "      - INITFORM: `~W`~%" (getf info :initform))
    (format out-stream "      - INITARG: `~W`~%" (getf info :initarg))
    (format out-stream "      - ACCESSOR: `~W`~%" (getf info :accessor))
    (format out-stream "      - READERS: `~W`~%" (getf info :readers))
    (format out-stream "      - WRITERS: `~W`~%" (getf info :writer))
    (awhen (getf info :docstring)
      (format out-stream "      - ~A~%" it))))


;; TODO condition?



(defun print-api-refs-as-markdown (api-refs)
  "print api-refs as markdown"
  (funcall *print-anchor-func* "" *api-refs-anchor*)
  (format *out-stream* "~A~%~%" *api-refs-heading*)
  (dolist (pkg api-refs)
    (funcall *print-anchor-func* "" (getf pkg :anchor))
    (format *out-stream* "## ~A~%~%" (getf pkg :text))
    (awhen (getf pkg :docstring)
      (format *out-stream* "~A~%" it))
    ;;
    (dolist (symb (getf pkg :symbols))
      (funcall *print-anchor-func* "" (getf symb :anchor))
      (format *out-stream* "### ~A~%~%" (getf symb :text))
      (format *out-stream* "- SCOPE: ~A~%"
              (symbol-scope (getf symb :name) (getf pkg :pkg-name)))
      (print-api-ref-body-as-markdown
       (getf symb :type) symb *out-stream*)
      (format *out-stream* "~%~%")
      )))


(defparameter *print-api-refs-func* #'print-api-refs-as-markdown)


(defun print-api-ref ()
  (log:info "PRINT API-REF ...")
  (funcall *print-api-refs-func* *api-refs*))



(defun extract-first-heading-from-markdown-file (pn)
  (iter (for line in-file
             (merge-pn-with-asdf-system-path pn *system-name*)
             using #'read-line)
    (setf line (str:trim line))
    (multiple-value-bind (match-string reg-strings)
        (cl-ppcre:scan-to-strings "^(#+)\\s*(.*)" line)
      (when match-string
        (return (aref reg-strings 1))))))



(defvar *toc-heading* "# Table of Contents")

(defvar *toc-title* "Table of Contents")

(defvar *toc-anchor* "TOC")


(defvar *api-refs-heading* "# APIs")

(defvar *api-refs-title* "APIs")

(defvar *api-refs-anchor* "API-REFS")

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
    (awhen (getf e :children)
      (print-toc-as-markdown it :level (1+ level))))
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
         (toc-appendf toc :text *api-refs-title*
                          :anchor *api-refs-anchor*
                          :children (api-refs->toc *api-refs*)
                          ))
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
  (log:info "SYSTEM DEF DIR: ~A" (system-definition-dir system-name))
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
      (log:info "GATHERING API-REFS ...")
      (let ((*api-refs* (gather-api-refs)))
        (expand-api-refs-for-toc *api-refs*)
        (log:info "BUILDING TOC ...")
        (let ((*toc* (build-toc)))
          ;;
          (log:info "ENSURE-DIR: ~a ..." *output-pn*)
          (ensure-directories-exist *output-pn*)
          (log:info "WRITING: ~a ..." *output-pn*)
          (uiop:with-output-file (*out-stream*
                                  *output-pn*
                                  :if-exists :supersede)
            (print-sections))))))
  (log:info "ALL GENERATED!"))




(defun ->upcase (val)
  (string-upcase (format nil "~A" val)))

(defun ->keyword (val)
  (intern (->upcase val) :keyword))


(defun symbol-scope (name pkg-name)
  (nth-value 1 (find-symbol (->upcase name)
                            (->keyword pkg-name))))



(defun remove-newlines (s)
  (remove #\Newline s))


(defun ->one-line-string (val)
  (-> val
      (format nil "~A" %)
      remove-newlines))


(defun expand-api-refs-for-toc (api-refs)
  (dolist (pkg api-refs)
    (rutils:nconcf
     pkg
     (let ((pkg-name (-> (getf pkg :pkg-name)
                         ->one-line-string)))
       (list :text (format nil "PACKAGE: ~A"
                           (api-ref-code-string pkg-name))
             :anchor (format nil "~APACKAGE-~A"
                             *api-ref-anchor-prefix*
                             pkg-name))))
    ;;
    (dolist (symb (getf pkg :symbols))
      (let ((type (getf symb :type))
            (name (getf symb :name)))
        (case type
          (:method
              (rutils:nconcf
               symb
               (let ((lambda-list-str (-> symb
                                          (getf % :info)
                                          (getf % :lambda-list)
                                          ->one-line-string)))
                 (list :text (format nil "~A: ~A ~A"
                                     type
                                     (api-ref-code-string name)
                                     (api-ref-code-string lambda-list-str))
                       :anchor (format nil "~A~A-~A-~A"
                                       *api-ref-anchor-prefix*
                                       type name lambda-list-str)))))
          (t
           (rutils:nconcf
            symb
            (list :text (format nil "~A: ~A" type
                                (api-ref-code-string name))
                  :anchor (format nil "~A~A-~A"
                                  *api-ref-anchor-prefix*
                                  type name)))))))
    ;;
    (api-refs-sort-toc-symbols (getf pkg :symbols)))
  (api-refs-sort-toc-packages api-refs))


(defun api-refs->toc (api-refs)
  (let ((results '()))
    (dolist (pkg api-refs)
      (let ((pkg-toc `(:text   ,(getf pkg :text)
                       :anchor ,(getf pkg :anchor)))
            (pkg-symbols '()))
        (dolist (symb (getf pkg :symbols))
          (rutils:nconcf pkg-symbols
                         (list (list :text (getf symb :text)
                                     :anchor (getf symb :anchor)))))
        (rutils:nconcf pkg-toc (list :children pkg-symbols))
        (rutils:nconcf results (list pkg-toc))))
    results))







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
