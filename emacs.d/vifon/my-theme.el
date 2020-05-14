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

(global-set-key (kbd "C-M-s-<") #'vifon/theme-light)
(global-set-key (kbd "C-M-s->") #'vifon/theme-dark)

(use-package solarized-theme
  :ensure t
  :config (setq solarized-scale-org-headlines nil
                solarized-scale-outline-headlines nil
                solarized-use-variable-pitch nil))


(defun vifon/daytime-solar-p ()
  (require 'cl-lib)
  (require 'seq)
  (require 'solar)
  (let* ((sunrise-sunset
          (mapcar (lambda (time)
                    (string-to-number
                     (let ((calendar-time-display-form '(24-hours minutes)))
                       (apply #'solar-time-string time))))
                  (seq-take (solar-sunrise-sunset (calendar-current-date))
                            2)))
         (sunrise (cl-first sunrise-sunset))
         (sunset (cl-second sunrise-sunset))
         (now (string-to-number (format-time-string "%H%M"))))
    (< sunrise
       now
       sunset)))

(defun vifon/daytime-p ()
  (let ((now (string-to-number (format-time-string "%H%M"))))
    (< 0800
       now
       1600)))

(defun vifon/theme-dwim (&optional no-disable)
  (interactive "P")
  (if (vifon/daytime-p)
      (vifon/theme-light no-disable)
    (vifon/theme-dark no-disable)))

(global-set-key (kbd "C-M-s-?") #'vifon/theme-dwim)

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
