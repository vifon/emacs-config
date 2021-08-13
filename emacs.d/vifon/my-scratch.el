(require 'scratch-mode)

(bind-key "," #'vifon/theme-light scratch-mode-map)
(bind-key "." #'vifon/theme-dark scratch-mode-map)
(bind-key "/" #'vifon/theme-dwim scratch-mode-map)
(bind-key "b" #'consult-buffer scratch-mode-map)
(bind-key "c" (lambda ()
                (interactive)
                (unless (bound-and-true-p chronos--buffer)
                  (require 'chronos)
                  (chronos-initialize))
                (switch-to-buffer chronos--buffer))
          scratch-mode-map)

(bind-key "C-c s" #'scratch-reset)
(setq initial-major-mode 'scratch-mode)

(provide 'my-scratch)
