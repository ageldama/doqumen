## Getting Started

1. Install it: Symlink `doqumen.asd` and `(PROGN
   (ASDF:CLEAR-CONFIGURATION) (QL:QUICKLOAD :doqumen))`

1. Write **`seed-plist`** variable:
   ```lisp
   (eval-when (:compile-toplevel :load-toplevel :execute)
     ;; Same keyword with name of ASDF-system:
     (setf (get :doqumen :doqumen)
           `(:sections (
                        ,#p"src/01-title.md"
                        :toc     ; <-- Place of TOC
                        (
                         ;; 1st item in a list is heading.
                         ,#p"src/02-intro.md"
                         ;; ...rests are subheadings:
                         ,#p"src/02-a-rationale.md"
                         ,#p"src/02-b-getting-started.md"
                         )
                        :api-ref ; Place API-Refs Here
                        :footer
                        ))))
   ```

1. Write .md files you've mentioned in `seed-plist`, and docstrings.

1. Generate: `(DOQUMEN:BUILD-DOC :doqumen)`

1. Enjoy the output: `docs/index.md`


