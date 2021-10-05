(when (featurep 'org)
  (set-register ?P `(file . ,(concat org-directory "/projects.org")))
  (set-register ?K `(file . ,org-default-notes-file)))
