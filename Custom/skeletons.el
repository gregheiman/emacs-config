(define-skeleton java-class-skeleton
  "Template for .java class"
  "Package Name: "
  "package " str "\n"
  "\n"
  "class " (file-name-sans-extension (file-name-nondirectory (buffer-file-name))) " {\n"
  "\t"_"\n"
  "}"
)

(define-skeleton header-guard-skeleton
    "Header guard skeleton for .h and .hpp files"
    nil
    "#ifndef _" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))) "_H\n"
    "#define _" (upcase (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))) "_H\n"
    "\n"
    _"\n"
    "\n"
    "#endif"
)
