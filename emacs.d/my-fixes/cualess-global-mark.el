(global-set-key (kbd "C-S-SPC") '(lambda ()
                                   (interactive)
                                   (cua-mode 1)
                                   (call-interactively 'cua-toggle-global-mark)))
(defadvice cua--deactivate-global-mark (after cua--deactivate-global-mark-and-cua-mode activate)
  (cua-mode 0))

(provide 'cualess-global-mark)
