(define-skeleton org-todo-skeleton
  "" ""
  "#+TODO: TODO(t) MAYBE(m) | INTEGRATION(i!) WAITING(w@) DONE(d!/@) ABORTED(a@/@)\n\n"
  _
  )

(define-skeleton org-latex-skeleton
  "" ""
  "#+BEGIN_LATEX\n"
  "\\setlength{\\parindent}{0em}\n"
  "\\linespread{1.3}\n"
  "#+END_LATEX\n")
