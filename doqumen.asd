(defsystem "doqumen"
  :version "0.0.1"
  :author "Jonghyouk Yun"
  :mailto "ageldama@gmail.com"
  :license "MIT"
  :depends-on (
               :uiop
               )
  :components ((:module "src"
                :serial t
                :components
                (
                 (:file "doqumen")
                 ))
               )
  :description "Yet another Lisp documentation generator, but it's way more dumber than others"
  )
