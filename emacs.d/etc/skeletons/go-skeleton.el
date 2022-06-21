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

(define-skeleton go-test-skeleton
  "" ""
  "package " (file-name-nondirectory
              (directory-file-name
               (file-name-directory
                (buffer-file-name))))
  "\n\n"
  "import \"testing\"\n"
  "\n"
  "func Test" _ "(t *testing.T) {\n"
  "\tt.Errorf(\"Not implemented\")\n"
  "}\n"
  )

(define-auto-insert "\\.go\\'" #'go-skeleton)
(define-auto-insert "/main\\.go\\'" #'go-main-skeleton)
(define-auto-insert "_test\\.go\\'" #'go-test-skeleton)

(provide 'go-skeleton)
