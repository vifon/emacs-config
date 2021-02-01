(define-skeleton go-skeleton
  "" ""
  "package " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"
  "\n"
  "func main() {\n"
  "\t" _ "\n"
  "}\n"
  )

(define-auto-insert "\\.go$" 'go-skeleton)
