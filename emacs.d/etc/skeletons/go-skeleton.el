(define-skeleton go-skeleton
  "" ""
  "package " (file-name-nondirectory
              (directory-file-name
               (file-name-directory
                (buffer-file-name))))
  "\n\n"
  _ "\n"
  )

(define-skeleton go-main-skeleton
  "" ""
  "package main\n"
  "\n"
  "func main() {\n"
  "\t" _ "\n"
  "}\n"
  )

(define-auto-insert "\\.go\\'" #'go-skeleton)
(define-auto-insert "/main\\.go\\'" #'go-main-skeleton)
