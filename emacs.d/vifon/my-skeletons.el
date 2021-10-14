;;; -*- lexical-binding: t; -*-

(dolist (skeleton-file (directory-files "~/.emacs.d/skeletons/" t "\\.elc?$"))
  (load skeleton-file))
