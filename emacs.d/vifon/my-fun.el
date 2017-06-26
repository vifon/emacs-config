(require 's)
(defun find-file-path ()
  "Find file using the PATH env var."
  (interactive)
  (let* ((program (read-shell-command "Program name: "))
         (path (executable-find (s-trim program))))
    (if path
        (let ((path (read-from-minibuffer "Find file: " path)))
          (when (and path (stringp path))
            (find-file path)))
        (error "No such program"))))

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
  (concat "~/tmp/scratch-"
          (format-time-string "%s_%Y-%m-%d")
          (when (not (string= name ""))
            (concat "--" name))
          "/"))

(defun scratch-dir (&optional use-git name)
  "Create an ad-hoc working directory and open it in dired.

Prefix argument initializes the Git repository."
  (interactive "P\nMName: ")
  (let ((directory (scratch-dir-path name)))
    (make-directory directory t)
    (when (file-symlink-p "~/scratch")
      (delete-file "~/scratch"))
    (make-symbolic-link directory "~/scratch" t)
    (when (car use-git)
      (require 'vc-git)
      (let ((default-directory directory))
        (vc-git-create-repo)))
    (find-file directory)))

(defun move-beginning-of-line-dwim (arg)
  (interactive "^p")
  (let ((old-point (point)))
    (back-to-indentation)
    (when (= old-point (point))
      (move-beginning-of-line arg))))

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

(provide 'my-fun)
