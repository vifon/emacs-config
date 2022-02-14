;;; -*- lexical-binding: t; -*-

(defun vifon/add-to-list-after (list-var old new &optional compare-fn)
  (let ((cmp (or compare-fn #'equal)))
    (cl-do ((x (symbol-value list-var) (cdr x)))
        ((or (null x)
             (funcall cmp (cadr x) new)))
      (when (funcall cmp (car x) old)
        (setf (cdr x) (cons new (cdr x))))))
  (symbol-value list-var))
