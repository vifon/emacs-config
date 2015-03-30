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
    (if (window-system)
        (start-process "my-urxvt" nil "urxvtcd")
        (start-process "my-tmux" nil
                       "tmux" "split-window" "-h"))))

(global-set-key (kbd "C-c x") 'run-term)
(global-set-key (kbd "M-o") 'run-term)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(global-set-key (kbd "C-c C-e")
                (defhydra epa-hydra
                  (:color blue)
                  ("e" epa-encrypt-region "epa-encrypt-region")
                  ("d" epa-decrypt-region "epa-decrypt-region")
                  ("s" epa-sign-region "epa-sign-region")
                  ("v" epa-verify-region "epa-verify-region")))
(eval-after-load "epa-mail"
  '(define-key epa-mail-mode-map (kbd "C-c C-e") 'epa-hydra/body))

(global-set-key (kbd "C-c =") 'diff-buffer-with-file)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "M-C-?") 'hippie-expand)

(global-set-key (kbd "C-c t")
                (defhydra text-filter-hydra
                  (:hint nil :color blue)
                  "
 ### Text utils ###

 delete-_t_railing-whitespace   delete-_m_atching-lines       _A_lign
 _a_ck                          delete-_n_on-matching-lines   align-_R_egexp

 _v_isual-line-mode        auto-_f_ill-mode   _d_iff-buffer-with-file
 toggle-t_r_uncate-lines   _l_inum-mode       aggressive-_i_ndent-mode"
                  ("t" delete-trailing-whitespace)
                  ("a" ack-and-a-half-same)
                  ("m" delete-matching-lines)
                  ("n" delete-non-matching-lines)
                  ("A" align)
                  ("R" align-regexp)
                  ("v" visual-line-mode :color red)
                  ("l" nlinum-mode :color red)
                  ("d" diff-buffer-with-file)
                  ("r" toggle-truncate-lines :color red)
                  ("f" auto-fill-mode :color red)
                  ("i" aggressive-indent-mode :color red)))

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
(use-package zop-to-char
  :bind (("M-Z" . zop-up-to-char)))

(global-set-key (kbd "C-x M-!") 'find-file-path)
(global-set-key (kbd "C-x M-j") 'dired-jump)

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


(global-set-key (kbd "C-c e")
                (defhydra vydra
                  (global-map "C-c e" :color pink)
                  "vi-hydra"

                  ("h" backward-char "left")
                  ("j" next-line "down")
                  ("k" previous-line "up")
                  ("l" forward-char "right")

                  ("f" scroll-up-command "PgDn")
                  ("b" scroll-down-command "PgUp")

                  ("v" er/expand-region "select")

                  ("0" move-beginning-of-line-dwim "BOL")
                  ("a" move-beginning-of-line-dwim "BOL")
                  ("e" move-end-of-line "EOL")

                  ("i" nil "quit")
                  ("q" nil "quit")))


(provide 'my-keys)
