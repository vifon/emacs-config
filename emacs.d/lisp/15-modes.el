;;; -*- lexical-binding: t; -*-

(define-minor-mode gofmt-global-mode
  "Automatically use gofmt on the saved Golang sources."
  :global t
  :lighter " gofmt"
  (if gofmt-global-mode
      (add-hook 'before-save-hook #'gofmt-before-save)
    (remove-hook 'before-save-hook #'gofmt-before-save)))

(define-minor-mode live-lossage-mode
  "Refresh and redisplay the lossage buffer after each command."
  :global t
  :lighter " lossage"
  (if live-lossage-mode
      (add-hook 'post-command-hook #'view-lossage)
    (remove-hook 'post-command-hook #'view-lossage)))
