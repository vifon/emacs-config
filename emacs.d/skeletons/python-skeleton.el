(define-skeleton python2-skeleton
  "" ""
  "#!/usr/bin/env python\n"
  "# -*- coding: utf-8 -*-\n\n"

  "from __future__ import print_function\n\n"

  "def main():\n    "
  _
  "\n\n"
  "if __name__ == \"__main__\":\n"
  "    main()"
  )

(define-skeleton python3-skeleton
  "" ""
  "#!/usr/bin/env python3\n"
  "\n\n"
  "def main():\n"
  "    " _ "\n"
  "\n\n"
  "if __name__ == \"__main__\":\n"
  "    main()"
  )

(define-skeleton python-async-skeleton
  "" ""
  "#!/usr/bin/env python3\n"
  "\n"
  "import asyncio\n"
  "\n\n"
  "async def main():\n"
  "    " _ "\n"
  "\n\n"
  "if __name__ == \"__main__\":\n"
  "    asyncio.run(main())"
  )

(define-skeleton python-setup-skeleton
  "" ""
  "#!/usr/bin/env python3\n"
  "\n"
  "from setuptools import setup, find_packages\n"
  "\n"
  "import " (setq v1 (skeleton-read "Root module: ")) "\n"
  "\n"
  "if __name__ == \"__main__\":\n"
  "    setup(\n"
  "        name=\"" v1 "\",\n"
  "        description=\"" (skeleton-read "Description: ") "\",\n"
  "        long_description=" v1 ".__doc__,\n"
  "        version=" v1 ".__version__,\n"
  "        author=" v1 ".__author__,\n"
  "        author_email=" v1 ".__email__,\n"
  "        license=" v1 ".__license__,\n"
  "        packages=find_packages(),\n"
  "        py_modules=[],\n"
  "        scripts=[],\n"
  "        entry_points={\n"
  "            'console_scripts': [\n"
  "                '" v1 " = " v1 ".__main__:main',\n"
  "            ],\n"
  "        },\n"
  "    )\n"
  )

(define-auto-insert "\\.py$" 'python3-skeleton)
(define-auto-insert "/setup.py$" 'python-setup-skeleton)
