(define-skeleton bash-strict-skeleton
  "" ""
  "#!/usr/bin/env bash\n"
  "\n"
  "SCRIPTDIR=\"$(cd \"$(dirname \"${BASH_SOURCE[0]}\")\" > /dev/null 2>&1 && pwd)\"\n"
  "\n"
  "set -o errexit -o nounset -o pipefail\n"
  "\n"
  _
  )

(define-auto-insert "\\.sh\\'" #'bash-strict-skeleton)

(provide 'shell-skeleton)
