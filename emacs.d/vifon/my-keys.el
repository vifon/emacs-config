(global-set-key (kbd "C-c i") 'auto-insert)
(global-set-key (kbd "C-c m") 'compile)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c R") 'auto-revert-mode)
(global-set-key (kbd "C-c z") 'winner-undo)
(global-set-key (kbd "<C-tab>") 'other-window)
(global-set-key (kbd "<C-S-iso-lefttab>") '(lambda (arg)
                                             (interactive "p")
                                             (other-window (- (or arg 1)))))
(global-set-key (kbd "<M-S-iso-lefttab>") 'indent-relative)
(global-set-key (kbd "M-S-SPC") '(lambda ()
                                   (interactive)
                                   (save-excursion
                                     (insert " "))))

(global-set-key (kbd "C-c o") 'find-file-at-point)

(defun run-term (&optional arg)
  (interactive "P")
  (let ((default-directory default-directory))
    (when arg
      (if (bound-and-true-p cppcm-build-dir)
          (cd cppcm-build-dir)
          (when (string-match "^.*/src/$" default-directory)
            (cd "../")
            (when (file-directory-p "build")
              (cd "build")))))
    (start-process "my-urxvt" nil "urxvtcd")))

(global-set-key (kbd "C-c x") 'run-term)
(global-set-key (kbd "M-o") 'run-term)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(global-set-key (kbd "C-c C-e e") 'epa-encrypt-region)
(global-set-key (kbd "C-c C-e d") 'epa-decrypt-region)
(global-set-key (kbd "C-c C-e s") 'epa-sign-region)
(global-set-key (kbd "C-c C-e v") 'epa-verify-region)

(global-set-key (kbd "C-c =") 'diff-buffer-with-file)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-C-?") 'hippie-expand)
(global-set-key (kbd "C-c t") '(lambda () (interactive)
                                 (delete-trailing-whitespace)
                                 (message "Trailing spaces deleted")))
(global-set-key (kbd "C-c s") '(lambda () (interactive)
                                 (switch-to-buffer "*scratch*")
                                 (cd "~/")))

(global-set-key (kbd "C-c d") 'delete-pair)
(define-key minibuffer-local-map (kbd "C-c c")
  '(lambda () (interactive) (insert "CMakeLists.txt")))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)
(defun conditionally-enable-paredit-mode ()
  "enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(use-package misc
  :bind (("M-z" . zap-up-to-char)
         ("C-M-y" . copy-from-above-maybe-line))
  :config (defun copy-from-above-maybe-line (arg)
            (interactive "P")
            (copy-from-above-command (if (consp arg)
                                         nil
                                         (or arg 1)))))

(global-set-key (kbd "C-x M-!") 'find-file-path)
(global-set-key (kbd "C-x M-j") 'dired-jump)
(global-set-key (kbd "M-s a") 'align)
(global-set-key (kbd "M-s r") 'align-regexp)

(defun toggle-selective-display (arg)
  (interactive "P")
  (if arg
      (set-selective-display arg)
      (if (equal (current-column)
                 selective-display)
          (set-selective-display 0)
          (set-selective-display (current-column)))))
(global-set-key [remap set-selective-display] 'toggle-selective-display)

(defun ido-insert-path (&optional arg)
  (interactive "P")
  (insert
   (funcall (if (not arg)
                'file-relative-name
                '(lambda (path)
                   (replace-regexp-in-string
                    (concat
                     "^" (getenv "HOME") "/")
                    "~/"
                    path)))
            (ido-read-file-name "Path: "))))
(global-set-key (kbd "C-c f") 'ido-insert-path)

(global-set-key [remap move-beginning-of-line] 'move-beginning-of-line-dwim)

(global-set-key (kbd "M-# q") 'quick-calc)
(global-set-key (kbd "M-# M-#") 'calc)

(global-set-key (kbd "<f9>")  'menu-bar-open)
(global-set-key [remap tmm-menubar] 'lacarte-execute-command)

(windmove-default-keybindings)


(key-chord-define-global "=w" 'whitespace-mode)
(key-chord-define-global "=d" 'dired-jump)
(key-chord-define-global "=g" 'vc-diff)
(key-chord-define-global "=f" 'diff-hl-margin-mode)
(key-chord-define-global "=q" 'nlinum-mode)
(key-chord-define-global "`\\" 'delete-frame)
(key-chord-define-global "[s" 'toggle-selective-display)
(key-chord-define-global "=t" 'ispell-change-dictionary)
(key-chord-define-global "=v" 'visual-line-mode)

(provide 'my-keys)
