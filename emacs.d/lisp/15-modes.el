;;; -*- lexical-binding: t; -*-

;;; Maintenance note to self:
;;;
;;; The modes are named with no prefixes for easy access and
;;; consistent hook/map/etc. names, but the aliases are created so
;;; that it's immediately obvious it's a custom thing when called from
;;; my init.el.

(define-minor-mode gofmt-global-mode
  "Automatically use gofmt on the saved Golang sources."
  :global t
  :lighter " gofmt"
  (if gofmt-global-mode
      (add-hook 'before-save-hook #'gofmt-before-save)
    (remove-hook 'before-save-hook #'gofmt-before-save)))
(defalias 'vifon/gofmt-global-mode 'gofmt-global-mode)

(define-minor-mode live-lossage-mode
  "Refresh and redisplay the lossage buffer after each command."
  :global t
  :lighter " lossage"
  (if live-lossage-mode
      (add-hook 'post-command-hook #'view-lossage)
    (remove-hook 'post-command-hook #'view-lossage)))
(defalias 'vifon/live-lossage-mode 'live-lossage-mode)
