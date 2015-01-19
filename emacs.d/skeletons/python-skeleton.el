(define-skeleton python-skeleton
  "" ""
  "#!/usr/bin/env python\n\n"

  "from __future__ import print_function\n\n"

  "def main(argv=None):\n    "
  _
  "\n\n"
  "if __name__ == \"__main__\":\n"
  "    from sys import argv\n"
  "    main(argv)"
  )

(define-auto-insert "\\.py$" 'python-skeleton)
