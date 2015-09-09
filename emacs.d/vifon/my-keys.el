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
      (cond ((bound-and-true-p cppcm-build-dir)
             (cd cppcm-build-dir))
            ((vc-root-dir)
             (cd (vc-root-dir)))
            (t (when (string-match "^.*/src/$" default-directory)
                 (cd "../")
                 (when (file-directory-p "build")
                   (cd "build"))))))
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

 _l_inum-mode              toggle-t_r_uncate-lines: %-3(bound-and-true-p truncate-lines)    aggressive-_i_ndent-mode: %-3(bound-and-true-p aggressive-indent-mode)
 _w_riteroom-mode: %-3(bound-and-true-p writeroom-mode)     variable-_p_itch-mode           _s_ort-lines
 _v_isual-line-mode: %-3(bound-and-true-p visual-line-mode)   auto-_f_ill-mode: %-3(not (equal auto-fill-function nil))           indent-_g_uide-mode: %-3(bound-and-true-p indent-guide-mode)

 _h_ttpd settings    fly_c_heck-mode etc."
                  ("t" delete-trailing-whitespace)
                  ("a" ack-and-a-half-same)
                  ("m" delete-matching-lines)
                  ("n" delete-non-matching-lines)
                  ("A" align)
                  ("R" align-regexp)
                  ("v" visual-line-mode :color red)
                  ("l" nlinum-mode :color red)
                  ("r" toggle-truncate-lines :color red)
                  ("f" auto-fill-mode :color red)
                  ("i" aggressive-indent-mode :color red)
                  ("w" writeroom-mode :color red)
                  ("s" sort-lines :color red)
                  ("p" variable-pitch-mode :color red)
                  ("g" indent-guide-mode :color red)
                  ("h" httpd-hydra/body :color blue)
                  ("c" fly*-hydra/body :color blue)
                  ("SPC" vydra/body)
                  ("q" nil)))

(defhydra httpd-hydra
  (:hint nil :color blue)
  "
### httpd settings ###

httpd-_s_tart    httpds-_S_top    _i_mpatient-mode"
  ("i" impatient-mode :color red)
  ("s" httpd-start)
  ("S" httpd-stop))

(defhydra fly*-hydra
  (:hint nil :color red)
  "
### fly*-mode ###

fly_c_heck-mode: %-3(bound-and-true-p flycheck-mode)    fly_s_pell-mode: %-3(bound-and-true-p flyspell-mode)
_S_emantic-mode: %-3(bound-and-true-p semantic-mode)"
  ("c" flycheck-mode)
  ("s" flyspell-mode)
  ("S" semantic-mode))

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

(defun kill-or-yank-whole-lines (&optional arg)
  (interactive "P")
  (if (consp arg)
      (save-excursion
        (beginning-of-line)
        (indent-region (point)
                       (progn
                         (yank)
                         (unless (equal
                                  (substring-no-properties (car kill-ring)
                                                           -1)
                                  "\n")
                           (insert "\n"))
                         (point))))
      (let ((kill-whole-line t)
            (saved-point (point))
            (saved-line (line-number-at-pos)))
        (beginning-of-line)
        (kill-line arg)
        (goto-char saved-point)
        (unless (equal saved-line (line-number-at-pos))
          (goto-line saved-line)
          (end-of-line)))))
(global-set-key [remap kill-sentence] #'kill-or-yank-whole-lines)

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
(key-chord-define-global "=a" 'shrink-all-windows-if-larger-than-buffer)

(defun shrink-all-windows-if-larger-than-buffer ()
  (interactive)
  (mapcar #'shrink-window-if-larger-than-buffer (window-list)))


(global-set-key (kbd "C-c e")
                (defhydra vydra
                  (:color pink)
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
                  ("q" nil "quit")
                  ("SPC" nil "quit")))


(provide 'my-keys)
