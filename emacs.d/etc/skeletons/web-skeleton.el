(define-skeleton html-skeleton
  "" ""
  "<!DOCTYPE html>\n"
  "<html lang=\""  (skeleton-read "Language: " "en") "\">\n"
  "  <head>\n"
  "    <meta charset=\"utf-8\">\n"
  "    <title>" (skeleton-read "Title: ") "</title>\n"
  "    <link rel=\"stylesheet\" type=\"text/css\" href=\""
  (skeleton-read "Stylesheet: " "style.css")
  "\">\n"
  "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n"
  "  </head>\n"
  "\n"
  "  <body>\n"
  "    " _ "\n"
  "  </body>\n"
  "</html>\n"
  )

(define-auto-insert "\\.html\\'" #'html-skeleton)
