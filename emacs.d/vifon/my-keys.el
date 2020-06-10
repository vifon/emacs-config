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

(global-set-key [remap just-one-space] #'cycle-spacing)

(defun run-term (&optional arg)
  (interactive "P")
  (let ((default-directory (if (derived-mode-p 'dired-mode)
                               (dired-current-directory)
                             default-directory)))
    (if (window-system)
        (start-process "alacritty" nil "alacritty")
      (call-process "tmux" nil 0 nil
                    "split-window" "-h"))))

(global-set-key (kbd "C-c x") 'run-term)
(global-set-key (kbd "M-o") 'run-term)

(global-set-key (kbd "C-c =") 'diff-buffer-with-file)

(global-set-key (kbd "M-C-?") 'hippie-expand)

(defun display-line-numbers-best ()
  (interactive)
  (call-interactively
   (if (fboundp #'display-line-numbers-mode)
       #'display-line-numbers-mode
     #'nlinum-mode)))

(global-set-key (kbd "C-c t")
                (defhydra my-hydra
                  (:hint nil :color blue)
                  "
 delete-_t_railing-whitespace    _A_lign
 r_G_                            align-_R_egexp

 _l_inum-mode              toggle-t_r_uncate-lines: %-3(bound-and-true-p truncate-lines)    aggressive-_i_ndent-mode: %-3(bound-and-true-p aggressive-indent-mode)
 _w_hitespace-mode: %-3(bound-and-true-p writespace-mode)    hl-_L_ine-mode                  _d_umb-jump-mode: %-3(bound-and-true-p dumb-jump-mode)
 _h_l-defined-mode: %-3(bound-and-true-p highlight-defined-mode)    _s_ort-lines                    c_o_mpare-windows
 _v_isual-line-mode: %-3(bound-and-true-p visual-line-mode)   auto-_f_ill-mode: %-3(not (equal auto-fill-function nil))           indent-_g_uide-mode: %-3(bound-and-true-p indent-guide-mode)

 _H_ttpd settings    fly_c_heck-mode etc."
                  ("t" delete-trailing-whitespace)
                  ("G" rg)
                  ("A" align)
                  ("R" align-regexp)
                  ("v" visual-line-mode :color red)
                  ("l" display-line-numbers-best :color red)
                  ("L" hl-line-mode :color red)
                  ("d" dumb-jump-mode :color red)
                  ("r" toggle-truncate-lines :color red)
                  ("f" auto-fill-mode :color red)
                  ("i" aggressive-indent-mode :color red)
                  ("w" whitespace-mode :color red)
                  ("s" sort-lines :color red)
                  ("g" indent-guide-mode :color red)
                  ("h" highlight-defined-mode :color red)
                  ("H" httpd-hydra/body :color blue)
                  ("c" fly*-hydra/body :color blue)
                  ("C" ispell-change-dictionary :color blue)
                  ("o" compare-windows :color red)
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
se_m_antic-mode: %-3(bound-and-true-p semantic-mode)    fly_S_pell-buffer
_i_spell-change-dictionary: %(when (boundp 'ispell-current-dictionary) ispell-current-dictionary)"
  ("c" flycheck-mode)
  ("s" flyspell-mode)
  ("S" flyspell-buffer :color blue)
  ("m" semantic-mode)
  ("i" ispell-change-dictionary))

(autoload 'gnus-dired-mode "gnus-dired" nil t)
(defhydra dired-submodes-hydra
  (:hint nil :color red)
  "
### dired submodes ###

dired-_a_sync^-^mode:    %-3(bound-and-true-p dired-async-mode)^^^   _f_ind-^d^ired
dired-_c_olla^p^se-mode: %-3(bound-and-true-p dired-collapse-mode)   _F_ind-^n^ame-dired
gnus-d^i^red-_m_ode:     %-3(bound-and-true-p gnus-dired-mode)^^^^   ^f^ind-_G_rep-dired
_v_c status: %-3(bound-and-true-p diff-hl-dired-mode)"
  ("c" dired-collapse-mode)
  ("a" dired-async-mode)
  ("m" gnus-dired-mode)
  ("f" find-dired :color blue)
  ("F" find-name-dired :color blue)
  ("G" find-grep-dired :color blue)
  ("v" diff-hl-dired-mode :color red))

(global-set-key (kbd "C-c d") 'delete-pair)

(if (version<= "24.4" emacs-version)
    ;; DEPRECATION
    (add-hook 'minibuffer-setup-hook
              (lambda () (if (eq this-command 'eval-expression)
                             (paredit-mode 1))))
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'paredit-mode))

(use-package misc
  :bind (("M-z" . zap-up-to-char)
         ("C-M-y" . copy-from-above-maybe-line))
  :config (defun copy-from-above-maybe-line (arg)
            (interactive "P")
            (copy-from-above-command (if (consp arg)
                                         nil
                                       (or arg 1)))))

(global-set-key (kbd "C-x M-!") #'find-file-path)
(autoload 's-trim "s")
(defun find-file-path ()
  "Find file using the PATH env var."
  (interactive)
  (let* ((program (read-shell-command "Program name: "))
         (path (executable-find (s-trim program))))
    (if path
        (let ((path (read-from-minibuffer "Find file: " path)))
          (when (and path (stringp path))
            (find-file path)))
      (error "No such program"))))

(defun toggle-selective-display (arg)
  (interactive "P")
  (if arg
      (set-selective-display arg)
    (if (not (zerop (or selective-display 0)))
        (set-selective-display nil)
      (set-selective-display (current-column)))))
(global-set-key [remap set-selective-display] #'toggle-selective-display)

(global-set-key [remap move-beginning-of-line]
                (defun move-beginning-of-line-dwim (arg)
                  (interactive "^p")
                  (let ((old-point (point)))
                    (back-to-indentation)
                    (when (= old-point (point))
                      (move-beginning-of-line arg)))))

(defun smart-kill-whole-lines (&optional arg)
  "Kill the whole line while keeping the point in place."
  (interactive "P")
  (let ((kill-whole-line t)
        (saved-point (point))
        (saved-line (line-number-at-pos)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char saved-point)
    (unless (equal saved-line (line-number-at-pos))
      (goto-char (point-min))
      (forward-line (1- saved-line))
      (end-of-line))))
(defun smart-yank-whole-lines ()
  "Yank and reindent the yanked text. Ensures the yanked text
ends with a newline."
  (interactive)
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
                     (point)))))
(global-set-key (kbd "M-k") #'smart-kill-whole-lines)
(global-set-key (kbd "M-K") #'smart-yank-whole-lines)

(global-set-key (kbd "<f9>") #'menu-bar-open)

(windmove-default-keybindings)


(defun shrink-all-windows-if-larger-than-buffer ()
  (interactive)
  (mapcar #'shrink-window-if-larger-than-buffer (window-list)))


(global-set-key (kbd "C-c e")
                (defhydra vydra
                  (:hint nil :color pink)
                  "
^↑^_b_↑  ^ ^_k_^ ^  ↔_v_↔
_0__a_↤  _h__i__l_  ↦_e_
^↓^_f_↓  ^ ^_j_^ ^   ^ ^
"
                  ("h" backward-char)
                  ("j" next-line)
                  ("k" previous-line)
                  ("l" forward-char)

                  ("f" scroll-up-command)
                  ("b" scroll-down-command)

                  ("v" er/expand-region)

                  ("0" move-beginning-of-line-dwim)
                  ("a" move-beginning-of-line-dwim)
                  ("e" move-end-of-line)

                  ("i" nil)
                  ("q" nil)
                  ("SPC" nil)))


(define-key isearch-mode-map (kbd "C-M-r") #'isearch-query-replace)


(provide 'my-keys)
