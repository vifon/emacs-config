;;; config-lib.el --- Facilities for loading the rest of my config.  -*- lexical-binding: t; -*-

;;;###autoload
(defun load-parts (directory &optional regexp)
  "Load all the Elisp files from DIRECTORY, in the lexicographical order.

REGEXP defaults to \"\\.elc?\\'\".

Inspired by: https://manpages.debian.org/stable/debianutils/run-parts.8.en.html"
  (interactive "D")
  (dolist (part (load-parts--gather directory regexp))
    (load part)))

(defun require-parts (directory &optional regexp)
  "The same as `load-parts' but uses `require' instead of `load'."
  (dolist (part (load-parts--gather directory regexp))
    (require (intern (file-name-nondirectory part))
             part)))

(defun load-parts--gather (directory &optional regexp)
  "Gather the files from DIRECTORY for `load-parts'.

REGEXP defaults to \"\\.elc?\\'\"."
  (setq regexp (or regexp "\\.elc?\\'"))
  (delete-dups
   (mapcar #'file-name-sans-extension
           (directory-files (file-name-as-directory directory)
                            t regexp))))

(defconst load-numbered-parts--regexp
  (rx string-start
      (= 2 digit)
      "-"
      (one-or-more (not "/"))
      ".el"
      (opt "c")
      string-end)
  "A regexp matching files like 10-name.el")

(defun load-numbered-parts (directory &optional max)
  "Load numbered config parts from DIRECTORY.

Optionally load only the parts up to the MAX numbered part."
  (dolist (part (load-parts--gather directory load-numbered-parts--regexp))
    (if (null max)
        (load part)
      (let* ((part-name (file-name-nondirectory part))
             (part-number-as-string (substring part-name 0 2))
             (part-number (string-to-number part-number-as-string)))
        (when (<= part-number max)
          (load part))))))

(provide 'config-lib)
