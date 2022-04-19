;;; -*- lexical-binding: t; -*-

(defun vifon/add-to-list-after (list-var old new &optional compare-fn)
  "Add NEW after OLD in the ordered list LIST-VAR.

OLD is compared with COMPARE-FN which defaults to `equal'.

NEW is not added if it already exists after OLD, also according
to COMPARE-FN, making this function idempotent."
  (let ((cmp (or compare-fn #'equal)))
    (cl-do ((x (symbol-value list-var) (cdr x)))
        ((null x))
      (when (and (funcall cmp (car x) old)
                 (not (funcall cmp (cadr x) new)))
        (setf (cdr x) (cons new (cdr x))))))
  (symbol-value list-var))

(defun vifon/string-make-clickable (string click-command)
  "Create a clickable STRING according to \"33.19.8 Defining Clickable Text\".

CLICK-COMMAND is what happens on click with the documentation
string of this command being the tooltip."
  (propertize
   string
   'mouse-face 'highlight
   'help-echo (concat "mouse-2: " (documentation click-command 'raw))
   'keymap (let ((map (make-sparse-keymap)))
             (define-key map [mouse-2] click-command)
             (define-key map (kbd "RET") click-command)
             (define-key map [follow-link] 'mouse-face)
             map)))
