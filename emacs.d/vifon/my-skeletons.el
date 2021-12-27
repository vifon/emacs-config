;;; -*- lexical-binding: t; -*-

(dolist (skeleton-file (directory-files "~/.emacs.d/etc/skeletons/" t "\\.elc?$"))
  (load skeleton-file))
