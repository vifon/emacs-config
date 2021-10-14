;;; -*- lexical-binding: t; -*-

;;; The code in this file consists of various code snippets found on
;;; the internet, possibly modified. I do not claim ownership on them.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun beautify-json ()
  (interactive)
  (let ((begin (if mark-active (min (point) (mark)) (point-min)))
        (end (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region begin end
                             "python -mjson.tool" (current-buffer) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; a modified version of this: http://blogs.fluidinfo.com/terry/2011/11/10/emacs-buffer-mode-histogram/#comment-680311213
(defun word-histogram-region (posBegin posEnd)
  "Display word histogram showing frequency of word occurrence."
  (interactive "r")
  (message "Counting...")
  (let* ((ht (make-hash-table :test 'equal))
         (totals '()))
    (save-excursion
      (goto-char posBegin)
      (while (and (< (point) posEnd)
                  (re-search-forward "\\(\\w+\\)\\W*" posEnd t))
        (puthash (match-string 1) (1+ (gethash (match-string 1) ht 0)) ht)))
    (maphash (lambda (key value)
               (setq totals (cons (list key value) totals)))
             ht)
    (setq totals (sort totals (lambda (x y) (> (cadr x) (cadr y)))))
    (with-output-to-temp-buffer "*word-histogram*"
      (princ (format "%d different words\n\n"
                     (length totals)))
      (dolist (item totals)
        (let
            ((key (car item))
             (count (cadr item))
             (maxcount (cadr (car totals))))
          (princ (format "%2d %20s %s\n" count key
                         (make-string (/ (* count (min 36 maxcount)) maxcount) ?+))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html

(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))

(bind-key "C-c l" #'narrow-or-widen-dwim)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ansi-color)
(defun ansi-colorize-buffer ()
  (interactive)
  (let ((was-read-only buffer-read-only))
    (read-only-mode 0)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode (if was-read-only 1 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(bind-key "M-Q" #'unfill-paragraph)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; http://oremacs.com/2015/03/05/testing-init-sanity/
(defun check-dotemacs ()
  (interactive)
  (require 'async)
  (async-start
   (lambda () (shell-command-to-string
               "emacs --batch --eval \"
(condition-case e
    (progn
      (load \\\"~/.emacs.d/init.el\\\")
      (message \\\"-OK-\\\"))
  (error
   (message \\\"ERROR!\\\")
   (signal (car e) (cdr e))))\""))
   `(lambda (output)
      (if (string-match "-OK-" output)
          (when ,(called-interactively-p 'any)
            (message "All is well"))
        (switch-to-buffer-other-window "*startup error*")
        (delete-region (point-min) (point-max))
        (insert output)
        (search-backward "ERROR!")))))
