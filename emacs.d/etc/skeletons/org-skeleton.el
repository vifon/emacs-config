(define-skeleton org-todo-skeleton
  "" ""
  "#+TODO: TODO(t) MAYBE(m) | INTEGRATION(i!) WAITING(w@) DONE(d!/@) ABORTED(a@/@)\n\n"
  _
  )

(define-skeleton org-latex-skeleton
  "" ""
  "#+LATEX_HEADER: \\usepackage{indentfirst}\n"
  "#+BEGIN_LATEX\n"
  "\\linespread{1.3}\n"
  "#+END_LATEX\n")

(define-skeleton org-timetable-skeleton
  "" ""
  "* Timetable

** Monthly
   #+BEGIN: clocktable :maxlevel 4 :scope file :block thismonth
   #+END:

** Weekly
   #+BEGIN: clocktable :maxlevel 4 :scope file :block thisweek
   #+END:

** Daily
   #+BEGIN: clocktable :maxlevel 4 :scope file :block thisweek :step day
   #+END:")

(provide 'org-skeleton)
