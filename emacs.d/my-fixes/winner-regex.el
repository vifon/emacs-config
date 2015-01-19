(when (require 'winner)
  (defvar winner-boring-buffers-regexp
    "\*[hH]elm.*\\|\*xhg.*\\|\*xgit.*")
  (defun winner-set1 (conf)
    ;; For the format of `conf', see `winner-conf'.
    (let* ((buffers nil)
           (alive
            ;; Possibly update `winner-point-alist'
            (cl-loop for buf in (mapcar 'cdr (cdr conf))
                     for pos = (winner-get-point buf nil)
                     if (and pos (not (memq buf buffers)))
                     do (push buf buffers)
                     collect pos)))
      (winner-set-conf (car conf))
      (let (xwins)                      ; to be deleted

        ;; Restore points
        (dolist (win (winner-sorted-window-list))
          (unless (and (pop alive)
                       (setf (window-point win)
                             (winner-get-point (window-buffer win) win))
                       (not (or (member (buffer-name (window-buffer win))
                                        winner-boring-buffers)
                                (string-match winner-boring-buffers-regexp
                                              (buffer-name (window-buffer win))))))
            (push win xwins)))          ; delete this window

        ;; Restore marks
        (save-current-buffer
          (cl-loop for buf in buffers
                   for entry = (cadr (assq buf winner-point-alist))
                   do (progn (set-buffer buf)
                             (set-mark (car entry))
                             (setf (winner-active-region) (cdr entry)))))
        ;; Delete windows, whose buffers are dead or boring.
        ;; Return t if this is still a possible configuration.
        (or (null xwins)
            (progn
              (mapc 'delete-window (cdr xwins)) ; delete all but one
              (unless (one-window-p t)
                (delete-window (car xwins))
                t))))))

  (defalias 'winner-set 'winner-set1))

