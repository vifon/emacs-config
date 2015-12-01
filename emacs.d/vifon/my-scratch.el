(setq initial-major-mode 'org-mode)

(defun scratch-reset-message ()
  (setq initial-scratch-message
        (if (file-exists-p "~/.emacs.d/scratch.org")
            (with-temp-buffer
              (insert-file-contents "~/.emacs.d/scratch.org")
              (buffer-string))
          "")))

(defun scratch-reset (&optional arg)
  (interactive "P")
  (switch-to-buffer "*scratch*")
  (cd "~/")
  (when (or arg (= (point-min) (point-max)))
    (erase-buffer)
    (insert (scratch-reset-message))
    (apply initial-major-mode nil)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(global-set-key (kbd "C-c s") #'scratch-reset)

(scratch-reset)

(provide 'my-scratch)
