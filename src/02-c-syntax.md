## Syntax

1. [Attached property](https://www.lispworks.com/documentation/HyperSpec/Body/f_get.htm#get)
   named `:DOQUMEN` is called "*seed plist*".
   1. could be specified with another property and at another symbol,
      with `(BUILD-DOC :your-system-name :seed-prop-name
      :ANOTHER-PROP-NAME :seed-symb :OF-ANOTHER-SYMBOL)`

1. The *seed plist* has only one property `:SECTIONS` (currently)

1. The `:SECTIONS` has a list as its' value in a sense of [Lisp
   Plist](https://www.lispworks.com/documentation/HyperSpec/Body/26_glo_p.htm#property_list)
   1. for example: `(:sections ( .... ))`

1. In the list of `:SECTIONS`, you could place one of:
   1. A pathname of text file.
      - Will be merged with pathname of `.asd` definition file,
      - Copied into the output file literally.

   1. Keywords: `:TOC`, `:API-REF`, `:FOOTER`
      - Doqumen has builtin supports above 3 keywords.
      - will place "Table of Contents", "API References", and the "Footer"
        in the output respectively.

      - Any other keywords could be placed, see "HACKS" section below.

   1. Nested list represents a section and its subsections.
      - for example, for a list of:
        `(:A (:B :B-1 :B-2) :C)`

      - will be rendered like:
        ```
        1. A
        2. B
           1. B-1
           2. B-2
        3. C
        ```

