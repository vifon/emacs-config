(define-skeleton c++-skeleton
  "" ""
  '(setq v1 (concat (file-name-base (buffer-file-name)) ".hpp"))
  (let ((header v1))
    (if (file-exists-p header)
        (concat "#include \"" header "\"\n\n"
                (when (progn
                        (setq v2 (skeleton-read "Namespace: "))
                        (not (string= "" v2)))
                  (concat "namespace " v2 " {\n\n")))
      (concat "#include <iostream>\n"
              "#include <string>\n\n"

              "int main(int argc, char *argv[])\n"
              "{\n    ")))
    _
    (if (not (file-exists-p v1))
        (concat "\n    return 0;\n"
                "}\n")
        (when (not (string= "" v2))
          (concat "\n\n} // namespace " v2))))


(define-skeleton c-skeleton
  "" ""
  '(setq v1 (concat (file-name-base (buffer-file-name)) ".h"))
  (let ((header v1))
    (if (file-exists-p header)
        (concat "#include \"" header "\"\n\n")
      (concat "#include <stdio.h>\n"
              "#include <stdlib.h>\n\n"

              "int main(int argc, char *argv[])\n"
              "{\n    ")))
  _
  (if (not (file-exists-p v1))
      (concat "\n    return 0;\n"
              "}\n")))

(define-skeleton c++-header-skeleton
  "" ""
  '(setq v1 (skeleton-read "Namespace: "))
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
  "#pragma once\n\n"
  _
  "\n"
  )

(define-skeleton c++-qt-skeleton
  "" ""
  "\n#include <QApplication>\n\n"

  "int main(int argc, char *argv[])\n"
  "{\n"
  "    QApplication app(argc, argv);\n\n"
  "    " _ "\n\n"
  "    return app.exec();\n"
  "}\n"
  )

(define-auto-insert "\\.\\(C\\|cc\\|cpp\\)\\'" #'c++-skeleton)
(define-auto-insert "\\.c\\'" #'c-skeleton)
(define-auto-insert "\\.\\(H\\|hpp\\)\\'" #'c++-header-skeleton)
(define-auto-insert "\\.h\\'" #'c-header-skeleton)

(provide 'c++-skeleton)
