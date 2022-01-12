;;; -*- lexical-binding: t; -*-

(require 'cl-lib)


(defun vifon/scratch-dir-path (name)
  (concat "~/scratch.d/scratch-"
          (format-time-string "%Y-%m-%d_%s")
          (when (not (string= name ""))
            (concat "--" name))
          "/"))

(define-obsolete-function-alias
  'scratch-dir 'vifon/make-scratch-dir "2021-11-04")

(defun vifon/make-scratch-dir (&optional name git)
  "Create an ad-hoc working directory at NAME and open it in dired.

Prefix argument GIT initializes it as a Git repository."
  (interactive "MScratch directory name: \nP")
  (let ((scratch (expand-file-name (vifon/scratch-dir-path name))))
    (make-directory scratch t)
    (when (file-symlink-p "~/scratch")
      (delete-file "~/scratch")
      (make-symbolic-link scratch "~/scratch" t))
    (when git
      (require 'vc-git)
      (let ((default-directory scratch))
        (vc-git-create-repo)))
    (dired scratch)))

(defun dired-import-ranger-tags ()
  (interactive)
  (let* ((ranger-tag-lines (with-temp-buffer
                             (insert-file-contents "~/.config/ranger/tagged")
                             (split-string (buffer-string) "\n")))
         (ranger-tags (mapcan (lambda (line)
                                (when (string-match "\\(?:\\(.\\):\\)?\\(.*\\)"
                                                    line)
                                  (list
                                   (cons (match-string 2 line)
                                         (string-to-char
                                          (or (match-string 1 line) "*"))))))
                              ranger-tag-lines)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if-let ((file (dired-get-filename nil t))
                 (tag (cdr (assoc file ranger-tags))))
            (let ((dired-marker-char tag))
              (dired-mark nil))
          (forward-line 1))))))

(defun vifon/image-yank ()
  (interactive)
  (when (or (zerop (buffer-size))
            (y-or-n-p "Buffer not empty. Proceed?"))
    (erase-buffer)
    (call-process "xclip" nil t nil "-selection" "clipboard" "-t" "image/png" "-o")
    (set-buffer-file-coding-system 'raw-text)
    (save-buffer)
    (normal-mode)
    (when (y-or-n-p "Kill the buffer?")
      (kill-buffer))))

(defun vifon/shrink-all-windows-if-larger-than-buffer ()
  (interactive)
  (mapcar #'shrink-window-if-larger-than-buffer (window-list)))

(defun vifon/add-to-list-after (list-var old new &optional compare-fn)
  (let ((cmp (or compare-fn #'equal)))
    (cl-do ((x (symbol-value list-var) (cdr x)))
        ((or (null x)
             (funcall cmp (cadr x) new)))
      (when (funcall cmp (car x) old)
        (setf (cdr x) (cons new (cdr x))))))
  (symbol-value list-var))
