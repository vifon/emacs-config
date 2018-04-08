(use-package exwm
  :ensure t
  :demand t
  :init (setq mouse-autoselect-window t
              focus-follows-mouse t)
  :config (progn
            (setq exwm-workspace-number 4)
            (add-hook 'exwm-update-class-hook
                      (lambda ()
                        (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                    (string= "gimp" exwm-instance-name))
                          (exwm-workspace-rename-buffer exwm-class-name))))
            (add-hook 'exwm-update-title-hook
                      (lambda ()
                        (when (or (not exwm-instance-name)
                                  (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                  (string= "gimp" exwm-instance-name))
                          (exwm-workspace-rename-buffer exwm-title))))
            (setq exwm-workspace-show-all-buffers t
                  exwm-layout-show-all-buffers t)

            (require 'exwm-systemtray)
            (exwm-systemtray-enable)

            (window-divider-mode 1)

            (setq exwm-input-global-keys
                  `(
                    (,(kbd "s-R") . exwm-reset)
                    (,(kbd "s-w") . exwm-workspace-switch)
                    ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
                    ,@(mapcar (lambda (i)
                                `(,(kbd (format "s-%d" i)) .
                                  (lambda ()
                                    (interactive)
                                    (exwm-workspace-switch-create ,i))))
                              (number-sequence 0 9))
                    (,(kbd "s-r") . (lambda (command)
	  	                      (interactive (list (read-shell-command "$ ")))
	  	                      (start-process-shell-command command nil command)))))
            ;; To add a key binding only available in line-mode, simply define it in
            ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
            (define-key exwm-mode-map (kbd "C-q") #'exwm-input-send-next-key)
            ;; The following example demonstrates how to use simulation keys to mimic
            ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
            ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
            ;; and DEST is what EXWM actually sends to application.  Note that both SRC
            ;; and DEST should be key sequences (vector or string).
            (setq exwm-input-simulation-keys
                  '(
                    ;; movement
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
                    ;; cut/paste.
                    ([?\C-w] . [?\C-x])
                    ([?\M-w] . [?\C-c])
                    ([?\C-y] . [?\C-v])
                    ;; search
                    ([?\C-s] . [?\C-f])))
            ;; You can hide the minibuffer and echo area when they're not used, by
            ;; uncommenting the following line.
            ;(setq exwm-workspace-minibuffer-position 'bottom)

            ;; Do not forget to enable EXWM. It will start by itself when things are
            ;; ready.  You can put it _anywhere_ in your configuration.
            (exwm-enable)))

(provide 'my-exwm)
