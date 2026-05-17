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


(defvar *seed-package-name-kw* nil)


(defun seed-plist-from-package-name-kw
    (&key
       (pkg-name-kw *seed-package-name-kw*)
       prop-name)
  (assert pkg-name-kw (pkg-name-kw))
  (let ((seed-plist (get pkg-name-kw prop-name)))
    seed-plist))



(defun build-doc
    (system-name
     &key
       seed-package-name-kw
       (seed-package-prop-name :doqumen)
       (output-file #p"docs/index.md"))
  ;;
  (asdf:load-system system-name)
  ;;
  (let* ((*seed-package-name-kw* seed-package-name-kw)
         (seed-plist (seed-plist-from-package-name-kw
                      :prop-name seed-package-prop-name)))
    (assert seed-plist (seed-plist))
    ;; TODO
    ))




;; TODO title
;; TODO sections
;; TODO api-refs
;; TODO api-refs: ordering?
;; TODO toc
