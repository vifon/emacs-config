;;; config-lib.el --- Facilities for loading the rest of my config.  -*- lexical-binding: t; -*-

(defun load-parts (directory &optional regexp)
  "Load all the Elisp files from DIRECTORY, in the lexicographical order.

REGEXP defaults to \"\\.elc?\\'\".

Inspired by: https://manpages.debian.org/stable/debianutils/run-parts.8.en.html"
  (interactive "D")
  (setq regexp (or regexp "\\.elc?\\'"))
  (dolist (part (delete-dups
                 (mapcar #'file-name-sans-extension
                         (directory-files (file-name-as-directory directory)
                                          t regexp))))
    (load part)))

(provide 'config-lib)
