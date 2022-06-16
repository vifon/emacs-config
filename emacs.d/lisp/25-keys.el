;;; -*- lexical-binding: t; -*-

(bind-key "C-c i" #'auto-insert)
(bind-key "C-c m" #'compile)
(unless (fboundp 'revert-buffer-quick)
  ;; In Emacs 28+ use \\[revert-buffer-quick] instead.
  (bind-key "C-c r" #'revert-buffer))
(bind-key "C-c R" #'auto-revert-mode)
(bind-key "C-c z" #'winner-undo)
(bind-key "<C-tab>" #'other-window)
(bind-key "<C-S-iso-lefttab>" (lambda (arg)
                                (interactive "p")
                                (other-window (- (or arg 1)))))
(bind-key [mode-line C-mouse-1] #'tear-off-window)
(bind-key "<M-S-iso-lefttab>" #'indent-relative)

(bind-key "C-x C-M-t" #'transpose-regions)
(bind-key "C-x M-e" #'pp-macroexpand-last-sexp)

(bind-key "C-c =" #'diff-buffer-with-file)

(bind-key "M-C-?" #'hippie-expand)
(bind-key [remap ispell-complete-word] #'complete-symbol)

(bind-key "C-c d" #'delete-pair)
(bind-key "C-M-r" #'isearch-query-replace isearch-mode-map)

(bind-key "<f9>" #'menu-bar-open)

(defun vifon/copy-from-above-maybe-line (arg)
  (interactive "P")
  (copy-from-above-command (if (consp arg)
                               nil
                             (or arg 1))))
(bind-key "C-M-y" #'vifon/copy-from-above-maybe-line)

(defun vifon/smart-kill-whole-lines (&optional arg)
  "Kill the whole line while keeping the point in place."
  (interactive "P")
  (let ((kill-whole-line t)
        (saved-point (point))
        (saved-line (line-number-at-pos)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char saved-point)
    (unless (equal saved-line (line-number-at-pos))
      (goto-char (point-min))
      (forward-line (1- saved-line))
      (end-of-line))))
(defun vifon/smart-yank-whole-lines ()
  "Yank and reindent the yanked text.  Ensures the yanked text
ends with a newline."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (indent-region (point)
                   (progn
                     (yank)
                     (unless (equal
                              (substring-no-properties (car kill-ring)
                                                       -1)
                              "\n")
                       (insert "\n"))
                     (point)))))
(bind-key "M-k" #'vifon/smart-kill-whole-lines)
(bind-key "M-K" #'vifon/smart-yank-whole-lines)


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

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(bind-key "M-Q" #'unfill-paragraph)
