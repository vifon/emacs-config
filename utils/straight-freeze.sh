#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

env EMACS_NO_LOCAL=1 emacs --batch \
      -l ~/.emacs.d/early-init.el \
      -l ~/.emacs.d/init.el \
      -f straight-freeze-versions
