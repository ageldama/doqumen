
## Your Own Custom Sections

Do you Remember that `:SECTIONS` of *seed plist* has a few special
sections like `:TOC`, `:API-REF`, and `:FOOTER`.

You could add your own custom sections, imagine `:MEDITATE` or
something. By writing and exporting functions: `DOQUMEN:PRINT-MEDITATE`
and `DOQUMEN:TOC-MEDITATE`.

Each function receives required arguments, for more detailed
information, please read the code of `DOQUMEN:PRINT-FOOTER` and
`DOQUMEN:TOC-FOOTER` and try your own hack. Yes, the `:FOOTER` is
implemented in this way.

Also, you could pass any arbitrary additional arguments by writing
this:
```lisp
|MEDITATE (:TEA "Drank")|
```

The custom section functions `PRINT-MEDITATE` and `TOC-MEDITATE` would
get `ARGS` as a list looks like `(LIST ... :TEA "Drank")`.


