(define-skeleton java-skeleton
  "" ""
  "public class " (file-name-nondirectory (file-name-sans-extension (buffer-file-name))) "\n"
  "{\n"
  "    public static void main(String[] args)\n"
  "    {\n"
  "        " _ "\n"
  "    }\n"
  "}\n"
  )

(define-auto-insert "\\.java$" 'java-skeleton)
