(define-skeleton ebuild-skeleton
  "" ""
  "# Copyright 1999-2018 Gentoo Foundation\n"
  "# Distributed under the terms of the GNU General Public License v2\n"
  "\n"
  "EAPI=7\n"
  "\n"
  "DESCRIPTION=\"\"\n"
  "HOMEPAGE=\"\"\n"
  "SRC_URI=\"\"\n"
  "\n"
  "LICENSE=\"\"\n"
  "SLOT=\"0\"\n"
  "KEYWORDS=\"~amd64 ~x86\"\n"
  "IUSE=\"\"\n"
  "\n"
  "DEPEND=\"\"\n"
  "RDEPEND=\"${DEPEND}\"\n"
  "BDEPEND=\"\"\n"
  _
  )
(define-auto-insert "\\.ebuild\\'" 'ebuild-skeleton)

(define-skeleton cruxpkg-skeleton
  "" ""
  "name=" (file-name-nondirectory (directory-file-name default-directory)) "\n"
  "version=" (skeleton-read "Version: ") "\n"
  "release=1\n"
  "source=(" _ ")\n"
  "\n"
  "build() {\n"
  "    cd $name-$version\n"
  (if (y-or-n-p "Does the project use ./configure?")
      (concat "    ./configure --prefix=$HOME/local --disable-nls\n"
              "    make\n"
              "    make DESTDIR=$PKG install\n")
    (concat "    make PREFIX=$HOME/local\n"
            "    make PREFIX=$HOME/local DESTDIR=$PKG install\n"))
  "    rm -rf $PKG/share/info\n"
  "}\n"
  )
(define-auto-insert "/Pkgfile\\'" 'cruxpkg-skeleton)
