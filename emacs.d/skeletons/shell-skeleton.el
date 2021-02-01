(define-skeleton bash-strict-skeleton
  "" ""
  "#!/usr/bin/env bash\n"
  "\n"
  "set -o errexit -o nounset -o pipefail\n"
  "\n"
  _
  )

(define-auto-insert "\\.sh$" 'bash-strict-skeleton)
