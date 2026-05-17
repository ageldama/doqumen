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

(defmethod type-keyword ((node docparser:record-node)) :record)

(defmethod type-keyword ((node docparser:type-node)) :type)






(setf idx (docparser:parse :raw-cffi-tcl9))

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
    (format t "~%")
    )
  ;;
  (format t "-----------------------------------------------------------~%")
  )





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
          (copy-file-into-stream in-pn *out-stream*)))))



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





