(define-skeleton latex-skeleton
  "" ""
  "\\documentclass[12pt]{article}\n"
  "\n"
  "\\usepackage[left=2.2cm,top=2.2cm,right=2.2cm,bottom=2.2cm]{geometry}\n"
  "\\usepackage{polski}\n"
  "\\usepackage[utf8]{inputenc}\n"
  "\\usepackage{indentfirst}\n"
  "\\usepackage{amsmath}\n"
  "\\linespread{1.3}\n"
  "\\makeatletter\n"
  "\n"
  "\\title{" (file-name-base (buffer-file-name)) "}\n"
  "\\author{" user-full-name "}\n"
  "\n"
  "\\begin{document}\n\n"
  _
  "\n\n\\end{document}\n"
  )

(define-auto-insert "\\.tex\\'" #'latex-skeleton)

(provide 'tex-skeleton)
