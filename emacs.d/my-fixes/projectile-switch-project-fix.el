(defun projectile-switch-project ()
  "Switch to a project we have seen before."
  (interactive)
  (let ((project-to-switch
         (funcall (lambda (prompt choices)
                    "Present a project tailored PROMPT with CHOICES."
                    (cond
                     ((eq projectile-completion-system 'ido) (ido-completing-read prompt choices))
                     (t (completing-read prompt choices))))
                  "Switch to which project: "
                  projectile-known-projects)))
    (dired project-to-switch)
    (let ((project-switched project-to-switch))
      (run-hooks 'projectile-switch-project-hook))))

(provide 'projectile-switch-project-fix)
