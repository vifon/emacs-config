;;; -*- lexical-binding: t; -*-

(defun vifon/toggle-selective-display (arg)
  (interactive "P")
  (if arg
      (set-selective-display arg)
    (set-selective-display (and (zerop (or selective-display 0))
                                (not (zerop (current-column)))
                                (current-column)))))
(bind-key [remap set-selective-display] #'vifon/toggle-selective-display)

(defun vifon/move-beginning-of-line (arg)
  (interactive "^p")
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= old-point (point))
      (move-beginning-of-line arg))))
(bind-key [remap move-beginning-of-line] #'vifon/move-beginning-of-line)

(defun vifon/indent-relative-dwim (&optional first-only unindented-ok)
  "Like `indent-relative' but disable `indent-tabs-mode' when aligning."
  (interactive "P")
  (let* ((at-indent-p (save-excursion
                        (skip-chars-backward " \t")
                        (eq (point) (point-at-bol))))
         (indent-tabs-mode (if at-indent-p
                               indent-tabs-mode
                             nil)))
    (indent-relative first-only unindented-ok)))
(bind-key [remap indent-relative] #'vifon/indent-relative-dwim)
