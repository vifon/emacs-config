;;; scratch-mode.el --- An opinionated major mode for a multi-purpose scratch buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Wojciech Siewierski

;; Author: Wojciech Siewierski
;; Keywords: convenience

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

;; To enable, set `initial-major-mode' to `scratch-mode':
;;   (setq initial-major-mode 'scratch-mode)

;; It is also recommended to bind `scratch-reset' to a key, for example:
;;   (global-set-key (kbd "C-c s") #'scratch-reset)

;;; Code:

(defgroup scratch-mode nil
  "A specialized mode for the scratch buffer.")

(defun scratch-reset ()
  "Reset the *scratch* buffer to its initial `scratch-mode' state."
  (interactive)
  (switch-to-buffer "*scratch*")
  (scratch-mode))

(defun scratch-self-insert (&optional pre post)
  "Produce a function calling `self-insert-command' in `scratch-mode'.

First switch to `lisp-interaction-mode' or call `PRE' instead if supplied.
Then call `self-insert-command' inserting whatever was pressed.
Finally call `POST' if it was supplied.

Initially intended as a quick way to switch to
`lisp-interaction-mode' and start a new S-expression, then it
was generalized."
  (lambda ()
    (interactive)
    (if pre
        (funcall pre)
      (lisp-interaction-mode))
    (self-insert-command 1)
    (when post
      (funcall post))))

(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'org-mode)
    (define-key map (kbd "e") #'lisp-interaction-mode)
    (define-key map (kbd "m") #'markdown-mode)
    (define-key map (kbd "i") (lambda ()
                                (interactive)
                                (find-file "~/.emacs.d/init.el")))
    (define-key map (kbd "I") (lambda ()
                                (interactive)
                                (find-file "~/.emacs.d/early-init.el")))
    (define-key map (kbd "C") (lambda ()
                                (interactive)
                                (find-library "scratch-mode")))
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "P") #'package-list-packages)
    (define-key map (kbd "SPC") #'notmuch)
    (define-key map (kbd "s") #'notmuch-search)
    (define-key map (kbd "M") #'notmuch-mua-new-mail)
    (define-key map (kbd "z") #'deft)
    (define-key map (kbd "C-c C-x C-j") #'org-clock-goto)
    (define-key map (kbd "j") #'org-clock-goto)
    (define-key map (kbd "J") #'org-mru-clock-select-recent-task)
    (define-key map (kbd "C-'") #'org-cycle-agenda-files)
    (define-key map (kbd "C-c C-w") #'org-refile)
    (define-key map (kbd "a") #'org-agenda)
    (define-key map (kbd "A") #'org-agenda)
    (define-key map (kbd "r") (lookup-key global-map (kbd "C-x r")))
    (define-key map (kbd "g") #'scratch-reset)
    (define-key map (kbd "S") #'scratch-dir)
    (define-key map (kbd "(") (scratch-self-insert))
    map))

(defcustom scratch-mode-key-hints
  '("o"
    "e"
    "m"
    ("i" . "~/.emacs.d/init.el")
    ("I" . "~/.emacs.d/early-init.el")
    "p"
    "SPC"
    "s"
    "M"
    "z"
    "a"
    "j"
    "J"
    "S")
  "The keymap hints to show in `scratch-mode'."
  :type '(repeat string))

(define-derived-mode scratch-mode special-mode "scratch"
  "A dedicated scratch buffer mode with commonly used commands bound."
  (setq font-lock-defaults
        '((("^\\(.*?\\):" 1 'bold)
           (": \\(.*\\)$" 1 'italic))))

  (setq cursor-type nil)

  (emacs-lock-mode 'kill)
  (cd "~/")

  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (elem scratch-mode-key-hints)
      (let* ((key (if (consp elem)
                      (car elem)
                    elem))
             (desc (if (consp elem)
                       (cdr elem)
                     (local-key-binding (kbd key)))))
        (insert (format "%s: %s\n" key desc)))))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (add-hook 'change-major-mode-hook
            (lambda ()
              (read-only-mode 0)
              (erase-buffer))
            nil t))

(provide 'scratch-mode)

;;; scratch-mode.el ends here
