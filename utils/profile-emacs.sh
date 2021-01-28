#!/usr/bin/env bash

set -o errexit -o nounset -o pipefail

emacs -Q \
      -l ~/.emacs.d/early-init.el \
      --eval "(profiler-start 'cpu)" \
      -l ~/.emacs.d/init.el \
      -f profiler-stop \
      -f profiler-report
