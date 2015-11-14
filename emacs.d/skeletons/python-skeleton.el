(define-skeleton python-skeleton
  "" ""
  "#!/usr/bin/env python\n\n"

  "from __future__ import print_function\n\n"

  "def main(argv=None):\n    "
  _
  "\n\n"
  "if __name__ == '__main__':\n"
  "    from sys import argv\n"
  "    main(argv)"
  )

(define-skeleton python-setup-skeleton
  "" ""
  "#!/usr/bin/env python3\n"
  "\n"
  "from distutils.core import setup\n"
  "\n"
  "import " (setq v1 (skeleton-read "Root module: ")) "\n"
  "\n"
  "if __name__ == '__main__':\n"
  "    setup(\n"
  "        name='" v1 "',\n"
  "        description=\"" (skeleton-read "Description: ") "\",\n"
  "        long_description=" v1 ".__doc__,\n"
  "        version=" v1 ".__version__,\n"
  "        author=" v1 ".__author__,\n"
  "        author_email=" v1 ".__email__,\n"
  "        license=" v1 ".__license__,\n"
  "        packages=['" v1 "'],\n"
  "        py_modules=[],\n"
  "        scripts=[],\n"
  "    )\n"
  )

(define-auto-insert "\\.py$" 'python-skeleton)
(define-auto-insert "/setup.py$" 'python-skeleton)
