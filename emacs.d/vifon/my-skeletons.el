(dolist (skeleton-file (directory-files "~/.emacs.d/skeletons/" t "\\.elc?$"))
  (load skeleton-file))

(provide 'my-skeletons)
