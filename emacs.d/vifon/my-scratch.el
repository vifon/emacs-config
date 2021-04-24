(require 'scratch-mode)

(bind-key "," #'vifon/theme-light scratch-mode-map)
(bind-key "." #'vifon/theme-dark scratch-mode-map)
(bind-key "/" #'vifon/theme-dwim scratch-mode-map)

(bind-key "C-c s" #'scratch-reset)
(setq initial-major-mode 'scratch-mode)

(provide 'my-scratch)
