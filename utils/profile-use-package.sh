#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

emacs -Q \
      --eval '(setq use-package-compute-statistics t)' \
      -l ~/.emacs.d/init.el \
      -f use-package-report
