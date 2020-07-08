(require 'scratch-mode)

(global-set-key (kbd "C-c s") #'scratch-reset)
(setq initial-major-mode 'scratch-mode)

(provide 'my-scratch)
