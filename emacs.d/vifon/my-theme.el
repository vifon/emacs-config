;;; -*- lexical-binding: t; -*-

(defun vifon/set-font ()
  (interactive)
  (ignore-errors
    (let* ((font-name "JetBrains Mono")
           (font-size "13")
           (font (concat font-name "-" font-size)))
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-frame-font font nil t))))

(defun vifon/switch-theme (background-mode new-theme &optional old-theme)
  "Change the current theme to NEW-THEME, disabling OLD-THEME.

BACKGROUND-MODE should be either `dark' or `light'."
  (when old-theme
    (disable-theme old-theme))
  (setq frame-background-mode background-mode)
  (load-theme new-theme 'no-confirm)
  (vifon/set-font))

(let ((vifon/theme-light 'solarized-light)
      (vifon/theme-dark  'solarized-dark))
  (defun vifon/theme-light (&optional no-disable)
    "Enable the preferred light theme.

Unless the prefix argument was passed, disable the current one beforehand."
    (interactive "P")
    (vifon/switch-theme 'light
                        vifon/theme-light
                        (unless no-disable
                          vifon/theme-dark)))

  (defun vifon/theme-dark (&optional no-disable)
    "Enable the preferred dark theme.

Unless the prefix argument was passed, disable the current one beforehand."
    (interactive "P")
    (vifon/switch-theme 'dark
                        vifon/theme-dark
                        (unless no-disable
                          vifon/theme-light))))

(bind-key "C-M-s-<" #'vifon/theme-light)
(bind-key "C-M-s->" #'vifon/theme-dark)

(use-package solarized-theme
  :straight t
  :config (setq solarized-scale-org-headlines nil
                solarized-scale-outline-headlines nil
                solarized-use-variable-pitch nil))


(defun vifon/solar-times ()
  (require 'seq)
  (require 'solar)
  (mapcar (lambda (time)
            (cons
             (string-to-number
              (let ((calendar-time-display-form '(24-hours minutes)))
                (apply #'solar-time-string time)))
             (let ((calendar-time-display-form '(24-hours ":" minutes)))
               (apply #'solar-time-string time))))
          (seq-take (solar-sunrise-sunset (calendar-current-date))
                    2)))

(defun vifon/daytime-solar-p ()
  "Check whether it's daytime according to the calculated sunrise
& sunset times.

See: Info node `(emacs) Sunrise/Sunset'."
  (let* ((sunrise-sunset (mapcar #'car (vifon/solar-times)))
         (sunrise (car sunrise-sunset))
         (sunset (cadr sunrise-sunset))
         (now (string-to-number (format-time-string "%H%M"))))
    (< sunrise
       now
       sunset)))

(defun vifon/daytime-p ()
  "Check whether it's daytime according to the hardcoded times."
  (let ((now (string-to-number (format-time-string "%H%M"))))
    (< 0800
       now
       1800)))

(defvar vifon/theme-timers nil)

(require 'cl-lib)
(defun vifon/theme-schedule-timers ()
  "Schedule the timers for automatic theme switching."
  (dolist (timer vifon/theme-timers)
    (cancel-timer timer))
  (let ((24h (* 24 60 60))
        (sunrise-sunset (mapcar #'cdr (vifon/solar-times))))
    (setq vifon/theme-timers
          (cl-mapcar
           (lambda (time theme)
             (run-at-time time 24h theme))
           sunrise-sunset
           '(vifon/theme-light
             vifon/theme-dark)))))

;;; Schedule only once per Emacs launch.  It shouldn't diverge that
;;; much during the Emacs instance lifetime for that to matter to
;;; recalculate the solar times.
(when (or (daemonp)
          (display-graphic-p))
  (vifon/theme-schedule-timers)
  (run-at-time "0:00" nil
               #'vifon/theme-dark))

(defun vifon/theme-dwim (&optional no-disable)
  (interactive "P")
  (if (vifon/daytime-solar-p)
      (vifon/theme-light no-disable)
    (vifon/theme-dark no-disable)))

(bind-key "C-M-s-?" #'vifon/theme-dwim)

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (defun vifon/theme-init-daemon (frame)
                (with-selected-frame frame
                  (vifon/theme-dwim 'no-disable))
                ;; Run this hook only once.
                (remove-hook 'after-make-frame-functions
                             #'vifon/theme-init-daemon)
                (fmakunbound 'vifon/theme-init-daemon)))
  (vifon/theme-dwim 'no-disable))


(provide 'my-theme)
