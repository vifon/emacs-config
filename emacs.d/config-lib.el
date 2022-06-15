;;; config-lib.el --- Facilities for loading the rest of my config.  -*- lexical-binding: t; -*-

;; Author: Wojciech Siewierski
;; Keywords: lisp, maint, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Facilities for loading the rest of my config.

;; Public functions provided:

;; (load-parts directory &optional regexp)
;; (require-parts directory &optional regexp)
;; (load-numbered-parts directory &optional max)

;;; Code:

;;;###autoload
(defun load-parts (directory &optional regexp)
  "Load all the Elisp files from DIRECTORY, in the lexicographical order.

REGEXP defaults to \"\\.elc?\\'\".

Inspired by: https://manpages.debian.org/stable/debianutils/run-parts.8.en.html"
  (interactive "D")
  (dolist (part (load-parts--gather directory regexp))
    (load part)))

(defun require-parts (directory &optional regexp)
  "The same as `load-parts' but uses `require' instead of `load'.

Should be used instead `load-parts' if loading a feature multiple
times is a concern."
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
;;; config-lib.el ends here
