;;; -*- lexical-binding: t; -*-

(defun vifon/set-font ()
  (interactive)
  (ignore-errors
    (let* ((font-name "JetBrains Mono")
           (font-size "13")
           (font (concat font-name "-" font-size)))
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-frame-font font nil t))))

(let ((vifon/theme-light 'solarized-light)
      (vifon/theme-dark  'solarized-dark))
  (defun vifon/theme-light (&optional no-disable)
    "Enable the preferred light theme.

Unless the prefix argument was passed, disable the current one beforehand."
    (interactive "P")
    (unless no-disable
      (disable-theme vifon/theme-dark))
    (setq frame-background-mode 'light)
    (load-theme vifon/theme-light 'no-confirm)
    (vifon/set-font))

  (defun vifon/theme-dark (&optional no-disable)
    "Enable the preferred dark theme.

Unless the prefix argument was passed, disable the current one beforehand."
    (interactive "P")
    (unless no-disable
      (disable-theme vifon/theme-light))
    (setq frame-background-mode 'dark)
    (load-theme vifon/theme-dark 'no-confirm)
    (vifon/set-font)))

(global-set-key (kbd "C-M-s-<") #'vifon/theme-light)
(global-set-key (kbd "C-M-s->") #'vifon/theme-dark)

(use-package solarized-theme
  :ensure t
  :config (setq solarized-scale-org-headlines nil
                solarized-scale-outline-headlines nil
                solarized-use-variable-pitch nil))

(vifon/theme-light 'no-disable)

(provide 'my-theme)
