(define-skeleton java-skeleton
  "" ""
  "public class " (file-name-base (buffer-file-name)) " {\n"
  "    public static void main(String[] args) {\n"
  "        " _ "\n"
  "    }\n"
  "}\n"
  )

(define-auto-insert "\\.java\\'" 'java-skeleton)
