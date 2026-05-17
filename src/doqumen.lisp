(defpackage #:doqumen
  (:use #:cl #:rutils)
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

(defvar *seed-package-name* nil)
(defvar *seed-prop-name* nil)

(defvar *seed-plist* nil)


(defun seed-plist-from-package-name
    (&key
       (pkg-name *seed-package-name*)
       prop-name)
  (assert pkg-name (pkg-name))
  (let ((seed-plist (get pkg-name prop-name)))
    seed-plist))



(defvar *output-pn* nil)
(defvar *out-stream* nil)

(defvar *docparser-index* nil)



;; TODO api-refs: printer function
;; TODO toc: printer-function



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





;; (setf idx (docparser:parse :raw-cffi-tcl9))
;; (setf idx (docparser:parse :tclish))

(defun xxx ()
  (docparser:do-packages (pkg idx)
    (format t "[PKG] ~a~%" (docparser:package-index-name pkg))
    (format t "~T[DOC] ~a~%~%" (docparser:package-index-docstring pkg))

    (docparser:do-nodes (node pkg)
      (format t "~T[NODE] ~a / ~a~%" 
              (docparser:node-name node)
              (type-keyword node)
              )
      (format t "~T~T[DOC] ~a~%"
              (docparser:node-docstring node))
      (format t "~T~T[INFO] ~a~%"
              (node-info-list node))
      (format t "~%")
      )
    ;;
    (format t "-----------------------------------------------------------~%")
    ))







(defun print-api-ref ()
  ;; TODO
  )


(defun print-toc ()
  ;; TODO
  )


(defun build-toc ()
  ;; TODO
  )


(defun print-sections ()
  ;; (format *out-stream* "~a ~A~%" *seed-plist* *output-pn*)
  (dolist (section (getf *seed-plist* :sections))
    (if (keywordp section)
        (case section
          (:toc (print-doc))
          (:api-ref (print-api-ref))
          ;; They told me I could be anything so I...:
          (t (funcall (-> section
                        string
                        (format nil "print-~a" %)
                        string-upcase
                        intern))
        ;; else: just a section
        (let ((in-pn
                (merge-pn-with-asdf-system-path section *system-name*)))
          (copy-file-into-stream in-pn *out-stream*)))))))



(defun build-doc
    (system-name
     &key
       seed-package-name
       (seed-prop-name :doqumen)
       (output-file #p"docs/index.md"))
  "Build it!"
  ;;
  (asdf:find-system system-name)
  ;;
  (let* ((*system-name* system-name)
         (*output-pn* (merge-pn-with-asdf-system-path output-file
                                                      system-name))
         (*seed-package-name* seed-package-name)
         (*seed-prop-name*    seed-prop-name)
         (*seed-plist* (seed-plist-from-package-name
                        :prop-name seed-prop-name)))
    (assert *seed-plist* (*seed-plist*))
    (ensure-directories-exist *output-pn*)
    (uiop:with-output-file (*out-stream*
                            *output-pn*
                            :if-exists :supersede)
      (let ((*docparser-index* (docparser:parse *system-name*)))
        (build-toc)
        (print-sections)
        ))))






;;; EXAMPLE: ... how it does "doqumen" itself:
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; ``seed-plist''
  (setf (get :doqumen :doqumen)
        `(
          ;; sections to be copied into:
          :sections
          (,#p"src/01-title.md"
              :toc                      ; special-section `toc'
              ,#p"src/02-intro.md"
              :api-ref                  ; special-section `api-ref'
              )
          )))





