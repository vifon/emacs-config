(define-skeleton p4config-skeleton
  "" ""
  "P4PORT=perforce" _ ":1666\n"
  "P4USER=" (user-login-name) "\n"
  "P4CLIENT=" (user-login-name) "_" system-name
  )

(define-auto-insert "/.p4config\\'" 'p4config-skeleton)
