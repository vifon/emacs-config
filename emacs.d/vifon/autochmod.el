(defun autochmod ()
  (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
      (progn
        (message (concat "Wrote " buffer-file-name))
        (dolist (pattern autochmod-extensions)
          (if (string-match pattern buffer-file-name)
              (shell-command (concat "chmod +x " buffer-file-name))
              (message (concat
                        "Wrote and made executable "
                        (buffer-file-name))))))
      (message (concat "Wrote " buffer-file-name))))

(add-hook 'after-save-hook 'autochmod)
(provide 'autochmod)
