;;; -*- lexical-binding: t; -*-

(defun run-term (&optional arg)
  (interactive "P")
  (let ((default-directory (if (derived-mode-p 'dired-mode)
                               (dired-current-directory)
                             default-directory)))
    (if (window-system)
        (call-process "alacritty" nil 0 nil)
      (call-process "tmux" nil 0 nil
                    "split-window" "-h"))))

(bind-key "C-c x" #'run-term)
(bind-key "M-o" #'run-term)

(use-package sane-term
  :straight t
  :config (add-hook 'term-mode-hook
                    (lambda ()
                      (yas-minor-mode 0)))
  :bind ("C-x T" . sane-term))
