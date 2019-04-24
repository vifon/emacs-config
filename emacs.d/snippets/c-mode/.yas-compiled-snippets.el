;;; Compiled snippets and support files for `c-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c-mode
                     '(("uni" "#include <unistd.h>" "unistd" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/unistd" nil nil)
                       ("union" "typedef union {\n        $0\n} ${1:name};" "union" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/union" nil nil)
                       ("case" "switch (${1:ch}) {\n       case ${2:const}:\n       ${3:a = b};\n       break;\n       ${4:default:\n       ${5:action}}\n}\n" "switch" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/switch" nil nil)
                       ("str" "#include <string.h>" "string" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/string" nil nil)
                       ("std" "#include <stdlib.h>\n" "stdlib" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/stdlib" nil nil)
                       ("io" "#include <stdio.h>" "stdio" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/stdio" nil nil)
                       ("printf" "printf(\"${1:%d}\\\\n\"${1:$(if (string-match \"%\" yas-text) \", \" \"\\);\")\n}$2${1:$(if (string-match \"%\" yas-text) \"\\);\" \"\")}" "printf" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/printf" nil nil)
                       ("packed" "__attribute__((__packed__))$0" "packed" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/packed" nil nil)
                       ("malloc" "${1:$(if (string-match \"^$\" yas-text) \"\" (concat \"(\" yas-text \"*)\"))}malloc(sizeof(${1:int})${2:})" "malloc(sizeof(...)*...);" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/malloc" nil nil)
                       ("infloop" "int _ = 1;\nfor (;_;) {\n    $0\n}" "while (1)" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/infloop" nil nil)
                       ("fori" "int ${1:i};\nfor ($1 = ${2:0}; $1 < ${3:N}; ++$1) {\n    $0\n}" "int ...; for ( ...; ...; ...) { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/fori" nil nil)
                       ("fopen" "FILE *${fp} = fopen(${\"file\"}, \"${r}\");" "FILE *fp = fopen(..., ...);" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/fopen" nil nil)
                       ("d" "#define $0" "define" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/define" nil nil)
                       ("compile" "// -*- compile-command: \"${1:gcc -Wall -o ${2:dest} ${3:file}}\" -*-" "compile" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/compile" nil nil)
                       ("ass" "#include <assert.h>\n$0" "assert" nil nil nil "/home/vifon/.emacs.d/snippets/c-mode/assert" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 00:56:11 2019
