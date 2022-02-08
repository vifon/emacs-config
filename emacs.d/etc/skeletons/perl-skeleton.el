(define-skeleton perl-skeleton
  "" ""
  "#!/usr/bin/env perl\n\n"

  "use warnings;\n"
  "use strict;\n"
  "use 5.010;\n\n"

  _
  )

(define-auto-insert "\\.pl\\'" #'perl-skeleton)

(provide 'perl-skeleton)
