(use-package exwm
  :ensure t
  :demand t
  :config (progn
            (setq exwm-workspace-number 4)
            (add-hook 'exwm-update-class-hook
                      (defun my-exwm-update-class-hook ()
                        (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                    (string= "gimp" exwm-instance-name))
                          (exwm-workspace-rename-buffer exwm-class-name))))
            (add-hook 'exwm-update-title-hook
                      (defun my-exwm-update-title-hook ()
                        (cond ((or (not exwm-instance-name)
                                   (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                   (string= "gimp" exwm-instance-name)
                                   (string= "Firefox" exwm-class-name))
                               (exwm-workspace-rename-buffer exwm-title))
                              ((string= "URxvt" exwm-class-name)
                               (exwm-workspace-rename-buffer (concat "URxvt: " exwm-title))))))

            (setq exwm-workspace-show-all-buffers nil
                  exwm-layout-show-all-buffers nil)

            (require 'exwm-systemtray)
            (exwm-systemtray-enable)

            (window-divider-mode 1)

            (setq exwm-input-global-keys
                  `((,(kbd "s-M") . (lambda () (interactive)
                                      (start-process-shell-command
                                       "ncmpcpp" nil
                                       "urxvtcd -e ncmpcpp-run")))
                    (,(kbd "s-d") . exwm-reset)
                    (,(kbd "s-c") . exwm-input-release-keyboard)
                    (,(kbd "s-w") . exwm-workspace-switch)
                    (,(kbd "s-b") . exwm-workspace-switch-to-buffer)
                    (,(kbd "s-f") . exwm-layout-toggle-fullscreen)
                    ,@(mapcar (lambda (arg)
                                (let ((key (car arg))
                                      (direction (cadr arg)))
                                  `(,(kbd (format "s-%s" key)) .
                                    ,(intern (concat "windmove-" direction)))))
                              '(("h" "left")
                                ("j" "down")
                                ("k" "up")
                                ("l" "right")))
                    (,(kbd "s-u") . (lambda ()
                                      (interactive)
                                      (split-window-below)
                                      (other-window 1)))
                    (,(kbd "s-o") . (lambda ()
                                      (interactive)
                                      (split-window-right)
                                      (other-window 1)))
                    (,(kbd "S-s-<return>") . exwm-floating-toggle-floating)
                    ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
                    ,@(mapcar (lambda (i)
                                `(,(kbd (format "s-%d" i)) .
                                  (lambda ()
                                    (interactive)
                                    (exwm-workspace-switch ,i))))
                              (number-sequence 0 9))
                    (,(kbd "s-r") . (lambda (command)
	  	                      (interactive (list (read-shell-command "$ ")))
	  	                      (start-process-shell-command command nil command)))
                    (,(kbd "s-R") . counsel-linux-app)))

            (define-key exwm-mode-map (kbd "C-q")
              (lambda ()
                (interactive)
                ;; Swallow the prefix argument to prevent happy accidents.
                (exwm-input-send-next-key 1)))

            (setq exwm-input-simulation-keys
                  `(;; movement
                    ([?\C-b] . [left])
                    ([?\M-b] . [C-left])
                    ([?\C-f] . [right])
                    ([?\M-f] . [C-right])
                    ([?\C-p] . [up])
                    ([?\C-n] . [down])
                    ([?\C-a] . [home])
                    ([?\C-e] . [end])
                    ([?\M-v] . [prior])
                    ([?\C-v] . [next])
                    ([?\C-d] . [delete])
                    ([?\C-k] . [S-end delete])
                    ([?\M-h] . [?\C-a])
                    ;; cut/paste.
                    ([?\M-w] . [?\C-c])
                    ([?\C-y] . [?\C-v])
                    ;; search
                    ([?\C-s] . [?\C-f])))

            (add-hook 'exwm-manage-finish-hook
                      (defun my-exwm-urxvt-simulation-keys ()
                        (when (and exwm-class-name
                                   (string= exwm-class-name "URxvt"))
                          (exwm-input-set-local-simulation-keys
                           `(,@(mapcar (lambda (key)
                                         `([,key] . [,key]))
                                       '(?\C-d
                                         ?\C-a
                                         ?\C-e
                                         ?\M-w
                                         ?\C-f
                                         ?\C-b
                                         ?\C-n
                                         ?\C-p
                                         ?\M-b
                                         ?\M-f
                                         ?\M-h
                                         ?\C-y
                                         ?\C-s
                                         ?\C-k
                                         ?\C-u)))))))

            (exwm-enable)))

(provide 'my-exwm)
