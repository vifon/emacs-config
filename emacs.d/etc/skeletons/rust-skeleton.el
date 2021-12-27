(define-skeleton rust-skeleton
  "" ""
  "fn main() {\n"
  "    " _ "\n"
  "}\n"
  )

(define-auto-insert "\\.rs\\'" #'rust-skeleton)
