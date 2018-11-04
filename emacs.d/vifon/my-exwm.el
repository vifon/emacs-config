;;; -*- lexical-binding: t; -*-

(use-package exwm
  :ensure t
  :demand t
  :config (progn
            (setq exwm-workspace-number 4)
            (add-hook 'exwm-update-class-hook
                      (defun my-exwm-update-class-hook ()
                        (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                    (string= "gimp" exwm-instance-name)
                                    (string= "Firefox" exwm-class-name))
                          (exwm-workspace-rename-buffer exwm-class-name))))
            (add-hook 'exwm-update-title-hook
                      (defun my-exwm-update-title-hook ()
                        (cond ((or (not exwm-instance-name)
                                   (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                                   (string= "gimp" exwm-instance-name)
                                   (string= "Firefox" exwm-class-name))
                               (exwm-workspace-rename-buffer exwm-title)))))

            (setq exwm-workspace-show-all-buffers nil
                  exwm-layout-show-all-buffers nil)

            (require 'exwm-systemtray)
            (exwm-systemtray-enable)

            (window-divider-mode 1)
            (display-battery-mode 1)
            (display-time-mode 1)

            ;; A dirty hack to display the current workspace on the
            ;; mode-line.  I prefer it to adding a new item to
            ;; global-mode-string due to a different placement on the
            ;; mode-line.  Semantically the meaning is close enough to
            ;; the original meaning of this variable, so I'll leave it
            ;; this way.
            (setq mode-line-frame-identification
                  '(:eval (propertize
                           (format "X:%d "
                                   exwm-workspace-current-index)
                           'face 'bold)))

            (defvar vifon/exwm-last-workspace-index nil)
            (advice-add 'exwm-workspace-switch :before
                        (defun vifon/exwm-save-last-workspace (&rest args)
                          (setq vifon/exwm-last-workspace-index
                                exwm-workspace-current-index)))
            (defun vifon/exwm-workspace-switch-or-last (super new-index &rest r)
              "If switching to the current workspace, switch to the last one instead."
              (apply super
                     (if (and vifon/exwm-last-workspace-index
                              (equal exwm-workspace-current-index
                                     new-index))
                         vifon/exwm-last-workspace-index
                       new-index)
                     r))
            (advice-add 'exwm-workspace-switch :around
                        #'vifon/exwm-workspace-switch-or-last)
            (defun vifon/exwm-last-workspace ()
              (interactive)
              (exwm-workspace-switch (or vifon/exwm-last-workspace-index
                                         exwm-workspace-current-index)))

            (defun my-exwm-mediaplayer ()
              (interactive)
              (start-process-shell-command
               "ncmpcpp" nil "urxvtcd -e ncmpcpp-run"))

            (defun my-exwm-next-workspace (arg)
              (interactive "p")
              (let* ((next-idx (+ exwm-workspace-current-index
                                  arg)))
                (exwm-workspace-switch next-idx)))
            (defun my-exwm-prev-workspace (arg)
              (interactive "p")
              (my-exwm-next-workspace (- arg)))

            (defun vifon/exwm-ibuffer (&optional other-window)
              (interactive "P")
              (let ((name (buffer-name)))
                (ibuffer other-window
                         "*exwm-ibuffer*"
                         '((mode . exwm-mode))
                         nil nil nil
                         '((mark " " name)))
                (ignore-errors (ibuffer-jump-to-buffer name))))

            (defun my-exwm-launch (command)
              (lambda ()
                (interactive)
	        (start-process-shell-command
                 command nil command)))

            (setq exwm-input-global-keys
                  `((,(kbd "s-M")        . my-exwm-mediaplayer)
                    (,(kbd "<s-escape>") . my-exwm-mediaplayer)
                    (,(kbd "s-d") . exwm-reset)
                    (,(kbd "s-c") . exwm-input-release-keyboard)
                    (,(kbd "s-w") . exwm-workspace-switch)
                    (,(kbd "s-b") . exwm-workspace-switch-to-buffer)
                    (,(kbd "s-x") . my-exwm-next-workspace)
                    (,(kbd "s-z") . my-exwm-prev-workspace)
                    (,(kbd "s-q") . vifon/exwm-last-workspace)
                    (,(kbd "<s-tab>") . vifon/exwm-ibuffer)
                    (,(kbd "s-SPC") . (lambda () (interactive) (call-process "urxvtcd")))
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
                    (,(kbd "s-Q") . (lambda () (interactive) (kill-buffer)))
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
                    (,(kbd "s-e") . counsel-linux-app)
                    (,(kbd "s-R") . rename-buffer)
                    (,(kbd "s-' s") . ,(my-exwm-launch "signal-desktop"))
                    (,(kbd "s-' t") . ,(my-exwm-launch "telegram"))
                    (,(kbd "s-' m") . ,(my-exwm-launch "notmuch-sync"))))

            (define-key exwm-mode-map (kbd "C-q")
              (lambda ()
                (interactive)
                ;; Swallow the prefix argument to prevent happy accidents.
                (exwm-input-send-next-key 1)))

            ;; The regular C-x 4 0 doesn't work in exwm-mode, let's do
            ;; it manually.
            (define-key exwm-mode-map (kbd "C-x 4 0")
              (lambda ()
                (interactive)
                (kill-buffer)
                (delete-window)))

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
                    ([?\C-w] . [?\C-x])
                    ([?\M-w] . [?\C-c])
                    ([?\C-y] . [?\C-v])
                    ;; search
                    ([?\C-s] . [?\C-f])))

            (add-hook 'exwm-manage-finish-hook
                      (defun my-exwm-urxvt-simulation-keys ()
                        (when exwm-class-name
                          (cond
                           ((string= exwm-class-name "URxvt")
                            (exwm-input-set-local-simulation-keys
                             (mapcar (lambda (key)
                                       `([,key] . [,key]))
                                     '(?\C-d
                                       ?\C-a
                                       ?\C-e
                                       ?\C-w
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
                                       ?\C-u))))
                           ((string= exwm-class-name "Firefox")
                            (exwm-input-set-local-simulation-keys
                             ;; TODO: Why is
                             ;; exwm-input-simulation-keys needed here
                             ;; but not for urxvt?
                             `(,@exwm-input-simulation-keys
                               ([?\C-w] . [?\C-w]))))))))

            (add-hook 'exwm-manage-finish-hook
                      (defun exwm--set-cwd ()
                        (cd (getenv "HOME"))))

            (exwm-enable)))

(provide 'my-exwm)
