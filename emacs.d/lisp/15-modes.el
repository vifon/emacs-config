;;; -*- lexical-binding: t; -*-

(define-minor-mode gofmt-global-mode
  "Automatically use gofmt on the saved Golang sources."
  :global t
  :lighter " gofmt"
  (if gofmt-global-mode
      (add-hook 'before-save-hook #'gofmt-before-save)
    (remove-hook 'before-save-hook #'gofmt-before-save)))
