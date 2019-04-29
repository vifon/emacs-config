(when (featurep 'org)
  (setq initial-major-mode 'org-mode))

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
  (emacs-lock-mode 'kill)
  (cd "~/")
  (cond
   ((equal arg '(16))
    ;; Emacs Lisp scratchpad.
    (erase-buffer)
    (insert "()")
    (lisp-interaction-mode)
    (set-buffer-modified-p nil)
    (goto-char (1+ (point-min))))
   ((or (equal arg '(4))
        (= (point-min) (point-max)))
    ;; General purpose org-mode scratchpad.
    (erase-buffer)
    (insert (scratch-reset-message))
    (funcall initial-major-mode)
    (set-buffer-modified-p nil)
    (goto-char (point-min)))))

(global-set-key (kbd "C-c s") #'scratch-reset)

(add-hook 'after-init-hook
          #'scratch-reset)

(provide 'my-scratch)
