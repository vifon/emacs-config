(defun c-c++-skeleton--get-file-comment ()
  (concat "/**\n"
          " * @file " (file-name-nondirectory (buffer-file-name)) "\n"
          " */\n"))

(define-skeleton c++-skeleton
  "" ""
  (c-c++-skeleton--get-file-comment)
  (let ((header (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".hpp")))
    (if (file-exists-p header)
        (concat "\n#include \"" header "\"\n\n"
                (when (progn
                        (setq v1 (skeleton-read "Namespace: "))
                        (not (string= "" v1)))
                  (concat "namespace " v1 " {\n\n")))
        (concat "\n#include <iostream>\n"
                "#include <string>\n\n"

                "int main(int argc, char *argv[])\n"
                "{\n    ")))
    _
    (if (not (file-exists-p (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".hpp")))
        (concat "\n    return 0;\n"
                "}\n")
        (when (not (string= "" v1))
          (concat "\n\n} // namespace " v1))))


(define-skeleton c-skeleton
  "" ""
  (c-c++-skeleton--get-file-comment)
  (let ((header (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".h")))
    (if (file-exists-p header)
        (concat "\n#include \"" header "\"\n\n")
        (concat "\n#include <stdio.h>\n"
                "#include <stdlib.h>\n\n"

                "int main(int argc, char *argv[])\n"
                "{\n    ")))
  _
    (if (not (file-exists-p (concat (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) ".h")))
        (concat "\n    return 0;\n"
                "}\n")))

(define-skeleton c++-header-skeleton
  "" ""
  (c-c++-skeleton--get-file-comment)
  (and (setq v1 (skeleton-read "Namespace: ")) nil)
  "#pragma once\n\n"
  (when (not (string= "" v1))
    (concat "namespace " v1 " {\n\n"))
  _
  (when (not (string= "" v1))
    (concat "\n\n} // namespace " v1))
  "\n"
  )

(define-skeleton c-header-skeleton
  "" ""
  (c-c++-skeleton--get-file-comment)
  "#pragma once\n\n"
  _
  "\n"
  )

(define-skeleton c++-qt-skeleton
  "" ""
  (c-c++-skeleton--get-file-comment)
  "\n#include <QApplication>\n\n"

  "int main(int argc, char *argv[])\n"
  "{\n"
  "    QApplication app(argc, argv);\n\n"
  "    " _ "\n\n"
  "    return app.exec();\n"
  "}\n"
  )

(define-skeleton c-ncurses-skeleton
  "" ""
  (c-c++-skeleton--get-file-comment)
  "\n#include <ncurses.h>\n\n"

  "int main(int argc, char *argv[])\n"
  "{\n"
  "    initscr();\n"
  "    cbreak();\n"
  "    noecho();\n"
  "    start_color();\n"
  "    keypad(stdscr, 1);\n\n    "

  _
  "\n\n    return endwin();\n"
  "}\n"
  )

(define-skeleton c++-catch-skeleton
  "" ""
  "#define CATCH_CONFIG_MAIN\n"
  "#include \"catch.hpp\"\n"
  )


(define-auto-insert "\\.\\(C\\|cc\\|cpp\\)$" 'c++-skeleton)
(define-auto-insert "\\.c$" 'c-skeleton)
(define-auto-insert "\\.\\(H\\|hpp\\)$" 'c++-header-skeleton)
(define-auto-insert "\\.h$" 'c-header-skeleton)
(define-auto-insert "/catch.cpp$" 'c++-catch-skeleton)
