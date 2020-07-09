(use-package el-patch
  :ensure t
  :config (setq el-patch-use-aggressive-defvar t))

(el-patch-feature dired)
(with-eval-after-load 'dired
  (el-patch-defun dired-copy-filename-as-kill (&optional arg)
    "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the Dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
    (interactive "P")
    (let ((string
           (or (dired-get-subdir)
               (mapconcat (el-patch-wrap 3
                            (if (equal arg '(16))
                                #'shell-quote-argument
                              #'identity))
                          (if arg
                              (cond ((zerop (prefix-numeric-value arg))
                                     (dired-get-marked-files))
                                    ((consp arg)
                                     (dired-get-marked-files t))
                                    (t
                                     (dired-get-marked-files
                                      'no-dir (prefix-numeric-value arg))))
                            (dired-get-marked-files 'no-dir))
                          " "))))
      (unless (string= string "")
        (if (eq last-command 'kill-region)
            (kill-append string nil)
          (kill-new string))
        (message "%s" string)))))

(el-patch-feature dired-subtree)
(with-eval-after-load 'dired-subtree
  (el-patch-defun dired-subtree--readin (dir-name)
    "Read in the directory.

Return a string suitable for insertion in `dired' buffer."
    (el-patch-wrap 2
      (let ((orig-buffer (current-buffer)))
        (with-temp-buffer
          (insert-directory dir-name
                            (el-patch-swap
                              dired-listing-switches
                              (buffer-local-value 'dired-actual-switches
                                                  orig-buffer))
                            nil t)
          (delete-char -1)
          (goto-char (point-min))
          (delete-region
           (progn (beginning-of-line) (point))
           (progn (forward-line
                   (if (save-excursion
                         (forward-line 1)
                         (end-of-line)
                         (looking-back "\\."))
                       3 1))
                  (point)))
          (insert "  ")
          (while (= (forward-line) 0)
            (insert "  "))
          (delete-char -2)
          (buffer-string))))))

(provide 'my-el-patch)
