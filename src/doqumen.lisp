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


(defun seed-plist-from-package-name
    (&key
       (pkg-name *seed-package-name*)
       prop-name)
  (assert pkg-name (pkg-name))
  (let ((seed-plist (get pkg-name prop-name)))
    seed-plist))


(defvar *seed-plist* nil)
(defvar *output-pn* nil)
(defvar *out-stream* nil)

(defvar *api-docs* nil)
(defvar *api-doc-list-sorter* #'identity)



(defun gather-api-doc  (sym sym-type)
  ;; TODO
  )


(defun gather-api-docs-of-package (package-name)
  ;; TODO
  )


(defun gather-api-docs ()
  ;; TODO
  )


(defun sort-api-docs ()
  (let ((sorted (funcall *api-doc-list-sorter*
                         *api-docs*)))
    (setf *api-docs* sorted)))


(defun print-api-ref ()
  ;; TODO
  )


(defun print-toc ()
  ;; TODO
  )


(defun print-sections ()
  ;; (format *out-stream* "~a ~A~%" *seed-plist* *output-pn*)
  (dolist (section (getf *seed-plist* :sections))
    (unless (keywordp section)
      (let ((in-pn
              (merge-pn-with-asdf-system-path section *system-name*)))
        (copy-file-into-stream in-pn *out-stream*)))))



(defun build-doc
    (system-name
     &key
       seed-package-name
       (seed-package-prop-name :doqumen)
       (output-file #p"docs/index.md"))
  ;;
  (asdf:load-system system-name)
  ;;
  (let* ((*system-name* system-name)
         (*output-pn* (merge-pn-with-asdf-system-path output-file
                                                      system-name))
         (*seed-package-name* seed-package-name)
         (*seed-plist* (seed-plist-from-package-name
                        :prop-name seed-package-prop-name)))
    (assert *seed-plist* (*seed-plist*))
    (ensure-directories-exist *output-pn*)
    (uiop:with-output-file (*out-stream*
                            *output-pn*
                            :if-exists :supersede)
      ;; gather & sort: api-refs
      (gather-api-docs)
      (sort-api-docs)
      ;; TODO build toc
      (print-sections)
      )))


;; TODO api-refs: sorting function
;; TODO api-refs: printer function

;; TODO toc: printer-function





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

          ;; packages and specific symbols to be included in `api-ref'
          :packages
          (:doqumen  ;; include whole package.
           ;; ...or selectively:
           ((asdf:load-system  . function)
            (uiop:timestamp<   . function)))
          )))





