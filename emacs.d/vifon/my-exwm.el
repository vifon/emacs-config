;;; -*- lexical-binding: t; -*-

(use-package exwm
  :straight t
  :defer t)

(defun vifon/exwm-activate ()
  (interactive)

  (require 'cl-lib)

  (setq exwm-workspace-number 6)
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

  (setq exwm-workspace-show-all-buffers t
        exwm-layout-show-all-buffers t)

  (setq epa-pinentry-mode 'loopback)

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
                 (format "X%s "
                         (exwm-workspace--position
                          (selected-frame)))
                 'face 'bold)))

  ;; XXX: Terrible hack.
  ;; The same thing for doom-modeline.  A terrible hack but
  ;; it will suffice for now.
  (doom-modeline-def-segment workspace-name
    (format "X%s " (exwm-workspace--position (selected-frame))))

  (require 'exwm-randr)
  (setq exwm-randr-workspace-monitor-plist
        (pcase (system-name)
          ("bifrost" (list 0 "DP-2"
                           1 "DP-4"
                           2 "DP-4"
                           3 "DP-4"
                           4 "HDMI-0"
                           5 "HDMI-0"))
          (_ nil)))
  (exwm-randr-enable)

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
    (start-process "alacritty" nil "alacritty" "-e" "ncmpcpp-auth"))

  (defun my-exwm-next-workspace (arg)
    (interactive "p")
    (let* ((next-idx (+ exwm-workspace-current-index
                        arg)))
      (exwm-workspace-switch next-idx)))
  (defun my-exwm-prev-workspace (arg)
    (interactive "p")
    (my-exwm-next-workspace (- arg)))


  (define-ibuffer-column exwm-class (:name "Class")
    (if (bound-and-true-p exwm-class-name)
        exwm-class-name
      ""))
  (define-ibuffer-column exwm-instance (:name "Instance")
    (if (bound-and-true-p exwm-instance-name)
        exwm-instance-name
      ""))
  (define-ibuffer-column exwm-urgent (:name "U")
    (if (bound-and-true-p exwm--hints-urgency)
        "U"
      " "))

  (defun vifon/exwm-ibuffer (&optional other-window)
    (interactive "P")
    (let ((name (buffer-name)))
      (ibuffer other-window
               "*exwm-ibuffer*"
               '((mode . exwm-mode))
               nil nil nil
               '((mark exwm-urgent
                       " "
                       (name 64 64 :left :elide)
                       " "
                       (exwm-class 10 -1 :left)
                       " "
                       (exwm-instance 10 -1 :left))))
      (ignore-errors (ibuffer-jump-to-buffer name))))

  (add-hook 'buffer-list-update-hook
            (defun vifon/exwm-remove-urgency ()
              (when (and (eq major-mode 'exwm-mode)
                         (eq (current-buffer) (window-buffer))
                         exwm--hints-urgency)
                (setf exwm--hints-urgency nil))))


  (defun my-exwm-launch (command)
    (lambda ()
      (interactive)
	  (start-process-shell-command
       command nil command)))

  (defun vifon/exwm-terminal ()
    (interactive)
    (let ((default-directory (if (derived-mode-p 'dired-mode)
                                 (dired-current-directory)
                               default-directory)))
      (start-process "alacritty" nil "alacritty")))

  (defun vifon/switch-to-last-buffer ()
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t)))

  (defun vifon/exwm-split (split-fun)
    (funcall split-fun)
    (other-window 1)
    (when (eq major-mode 'exwm-mode)
      ;; Needed for the original window's buffer to not get
      ;; auto-switched.  If there are two windows with the
      ;; same EXWM buffer at the end of a command, one of
      ;; them gets its buffer buried.  A few EXWM versions
      ;; ago it happened to be the one we wanted, but this
      ;; is no longer the case.  This should take care of it
      ;; and it should no longer be dependent on the
      ;; EXWM internals.
      (switch-to-buffer (other-buffer (current-buffer) t))))

  (defun vifon/exwm-split-below ()
    (interactive)
    (vifon/exwm-split #'split-window-below))

  (defun vifon/exwm-split-right ()
    (interactive)
    (vifon/exwm-split #'split-window-right))

  (setq exwm-input-global-keys
        `((,(kbd "s-M")        . my-exwm-mediaplayer)
          (,(kbd "<s-escape>") . my-exwm-mediaplayer)
          (,(kbd "<C-s-tab>") . vifon/switch-to-last-buffer)
          (,(kbd "s-d") . exwm-reset)
          (,(kbd "s-c") . exwm-input-release-keyboard)
          (,(kbd "s-w") . exwm-workspace-switch)
          (,(kbd "s-x") . my-exwm-next-workspace)
          (,(kbd "s-z") . my-exwm-prev-workspace)
          (,(kbd "s-q") . vifon/exwm-last-workspace)
          (,(kbd "<s-tab>") . vifon/exwm-ibuffer)
          (,(kbd "s-SPC") . vifon/exwm-terminal)
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
          (,(kbd "s-u") . vifon/exwm-split-below)
          (,(kbd "s-o") . vifon/exwm-split-right)
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
          (,(kbd "s-R") . rename-buffer)
          (,(kbd "s-' s") . ,(my-exwm-launch "signal-desktop"))
          (,(kbd "s-' t") . ,(my-exwm-launch "telegram"))
          (,(kbd "s-' m") . ,(my-exwm-launch "notmuch-sync"))))

  (bind-key "C-q"
            (lambda ()
              (interactive)
              ;; Swallow the prefix argument to prevent happy accidents.
              (exwm-input-send-next-key 1))
            exwm-mode-map)

  ;; The regular C-x 4 0 doesn't work in exwm-mode, let's do
  ;; it manually.
  (bind-key "C-x 4 0"
            (lambda ()
              (interactive)
              (kill-buffer)
              (delete-window))
            exwm-mode-map)

  (bind-key "C-c C-M-m" #'exwm-workspace-move
            exwm-mode-map)

  (cl-mapcar (lambda (c n)
               (define-key exwm-mode-map (kbd (format "s-%c" c))
                 (lambda ()
                   (interactive)
                   (exwm-workspace-move-window n)
                   (exwm-workspace-switch n))))
             '(?\) ?! ?@ ?# ?$ ?% ?^ ?& ?* ?\()
             (number-sequence 0 9))

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
            (defun my-exwm-term-simulation-keys ()
              (when exwm-class-name
                (cond
                 ((or (string= exwm-class-name "URxvt")
                      (string= exwm-class-name "Alacritty"))
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

  (exwm-enable))
