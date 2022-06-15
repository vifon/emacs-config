;;; -*- lexical-binding: t; -*-

(defun vifon/scratch-dir-path (name)
  (file-name-as-directory
   (expand-file-name
    (concat "scratch-"
            (format-time-string "%Y-%m-%d_%s")
            (when (and name (not (string= name "")))
              (concat "--" name)))
    "~/scratch.d")))

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
  (mapc #'shrink-window-if-larger-than-buffer (window-list)))

(defun ansi-colorize-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max)))
  (set-buffer-modified-p nil))

(autoload 'grep-mode "grep" nil t)
(defun ansi-color-grep-mode ()
  (interactive)
  (ansi-colorize-buffer)
  (grep-mode))

;;; Experimental!
(defun vifon/python-yank (&optional arg)
  "`yank' and reindent when yanking whole blocks."
  (interactive "*P")
  (setq this-command 'yank)
  (if (and (python-info-current-line-empty-p)
           (> (current-indentation) 0))
      (let* ((target-indent (current-indentation))
             (yanked-text (with-temp-buffer
                            (yank arg)
                            ;; If the yanked text doesn't end with a newline, assume
                            ;; it's not a whole block so don't try to reindent it.
                            (when (eq (char-after (1- (point-max))) ?\n)
                              (goto-char (point-min))
                              (let ((min-indent (current-indentation)))
                                (while (progn (forward-line)
                                              (not (eobp)))
                                  (setq min-indent (min (current-indentation)
                                                        min-indent)))
                                (indent-rigidly (point-min) (point-max)
                                                (- target-indent min-indent))))
                            (buffer-string))))
        (delete-horizontal-space t)
        (insert yanked-text))
    (yank arg)))
