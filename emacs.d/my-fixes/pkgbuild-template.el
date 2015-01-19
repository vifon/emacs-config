(defcustom pkgbuild-template
"# Maintainer: %s <%s>
pkgname=%s  
pkgver=VERSION
pkgrel=1 
pkgdesc=\"\"
url=\"\"
arch=('i686' 'x86_64')
license=('GPL')
depends=()
makedepends=()
conflicts=()
replaces=()
backup=()
install=
source=($pkgname-$pkgver.tar.gz)
md5sums=()

_gitroot=\"git://github.com/USERNAME/NAME.git\"
_gitname=\"NAME\"

build() {
  cd \"$srcdir\"
  msg \"Connecting to GIT server....\"

  if [ -d $_gitname ]; then
    cd $_gitname && git pull origin --depth=1
    msg \"The local files are updated.\"
  else
    git clone $_gitroot $_gitname --depth=1
  fi

  msg \"GIT checkout done or server timeout\"
  rm -rf \"$srcdir/$_gitname-build\"
  cp -r  \"$srcdir/$_gitname\" \"$srcdir/$_gitname-build\"
  cd     \"$srcdir/$_gitname-build\"
  make || return 1
}

package() {
  cd \"$srcdir/$_gitname-build\"
  install -D -m CHMOD FILE  $pkgdir/DESTINATION

}"
  "Template for new PKGBUILDs"
  :type 'string
  :group 'pkgbuild)
