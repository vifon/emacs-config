#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

PACKAGE="$1"

emacs -Q \
      --eval '(with-eval-after-load "'"$PACKAGE"'" (setq debug-on-next-call t))' \
      -l ~/.emacs.d/early-init.el \
      -l ~/.emacs.d/init.el
