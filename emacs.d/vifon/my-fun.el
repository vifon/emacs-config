;;; used by yasnippet
(defun get-c-arg-names (arg-list)
  (mapconcat
   (lambda (arg)
     (replace-regexp-in-string "^.*?\\(\\w*\\)[[:blank:]]*$" "\\1" arg))
   (split-string text ",")
   ", "))
(defun get-c-arg-types (arg-list)
  (mapconcat
   (lambda (arg)
     (replace-regexp-in-string "^[[:blank:]]*\\(.*?\\)[[:blank:]]*\\w*[[:blank:]]*$" "\\1" arg))
   (split-string text ",")
   ", "))

(defun scratch-dir-path (name)
  (concat "~/scratch.d/scratch-"
          (format-time-string "%Y-%m-%d_%s")
          (when (not (string= name ""))
            (concat "--" name))
          "/"))

(defun scratch-dir (&optional use-git name)
  "Create an ad-hoc working directory and open it in dired.

Prefix argument initializes the Git repository."
  (interactive "P\nMName: ")
  (let ((directory (expand-file-name (scratch-dir-path name))))
    (make-directory directory t)
    (when (file-symlink-p "~/scratch")
      (delete-file "~/scratch"))
    (make-symbolic-link directory "~/scratch" t)
    (when (car use-git)
      (require 'vc-git)
      (let ((default-directory directory))
        (vc-git-create-repo)))
    (find-file directory)))

(defun call-with-default-completing-read (oldfun &rest args)
  "Call `oldfun' with a `completing-read-function' set to `completing-read-default'.

`ivy-completing-read' and possibly other custom
`completing-read-function' values do interfere with some
commands."
  (let ((completing-read-function #'completing-read-default))
    (apply oldfun args)))

(defun cc-extract ()
  "Save the full function body to the kill-ring and replace it with a declaration."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (let* ((function-body (buffer-substring-no-properties
                             (point-min)
                             (point-max)))
             (function-indent ;get the absolute indent of the function body
              (replace-regexp-in-string "^\\([[:blank:]]*\\).*" "\\1"
                                        (car (split-string function-body "\n"))))
             (deindented-function-body
              (replace-regexp-in-string (concat "^"
                                                function-indent)
                                        ""
                                        function-body)))
        (kill-new deindented-function-body)
        (goto-char (1- (point-max)))    ;1- because we do not want to
                                        ;delete the final newline
        (let ((body-end (point)))
          (backward-sexp)               ;go to the beginning of the body
          (let ((body-begin (point)))
            (delete-region body-begin
                           body-end))
          (delete-horizontal-space)
          (let ((tmp (point)))
            (skip-chars-backward " \n\t")
            (delete-region (point)      ;remove the leftover whitechars
                           tmp))
          (insert ";"))))))

(defun cc-headerize ()
  "Save the function header to the kill-ring."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-defun)
      (goto-char (point-min))
      (skip-chars-forward " \n\t")
      (let ((function-begin (point)))
        (goto-char (point-max))
        (backward-sexp)
        (skip-chars-backward " \n\t")
        (let ((function-header
               (buffer-substring-no-properties function-begin
                                               (point))))
          (kill-new function-header)
          (message "Header copied: %s" function-header))))))

(defun flags-if-supported (command &rest flags)
  "Return each flag in FLAGS unchanged if COMMAND supports it
according to the manpage, otherwise return nil in its place.

The flag is considered supported if it appears anywhere in the
manpage."
  (with-temp-buffer
    (process-file "man" nil t nil command)
    (mapcar (lambda (flag)
              (goto-char (point-min))
              (when (re-search-forward (concat "\\_<"
                                               (regexp-quote flag)
                                               "\\_>")
                                       nil t)
                flag))
            flags)))

(require 'cl-lib)
(defun flags-nonportable (portable-flags command &rest nonportable-flags)
  "Combine PORTABLE-FLAGS and the supported NONPORTABLE-FLAGS
into a single string with `combine-and-quote-strings'.

See also: `flags-if-supported'."
  (combine-and-quote-strings
   (cl-delete-if
    nil (cons portable-flags
              (apply #'flags-if-supported
                     command
                     nonportable-flags)))))

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

(provide 'my-fun)
