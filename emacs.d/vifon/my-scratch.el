(require 'scratch-mode)

(define-key scratch-mode-map (kbd ",") #'vifon/theme-light)
(define-key scratch-mode-map (kbd ".") #'vifon/theme-dark)
(define-key scratch-mode-map (kbd "/") #'vifon/theme-dwim)

(global-set-key (kbd "C-c s") #'scratch-reset)
(setq initial-major-mode 'scratch-mode)

(provide 'my-scratch)
