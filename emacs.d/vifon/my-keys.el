(bind-key "C-c i" #'auto-insert)
(bind-key "C-c m" #'compile)
(bind-key "C-c r" #'revert-buffer)
(bind-key "C-c R" #'auto-revert-mode)
(bind-key "C-c z" #'winner-undo)
(bind-key "<C-tab>" #'other-window)
(bind-key "<C-S-iso-lefttab>" (lambda (arg)
                                (interactive "p")
                                (other-window (- (or arg 1)))))
(bind-key [mode-line C-mouse-1] #'tear-off-window)
(bind-key "<M-S-iso-lefttab>" #'indent-relative)

(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)

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

(bind-key "C-c =" #'diff-buffer-with-file)

(bind-key "M-C-?" #'hippie-expand)

(defun display-line-numbers-best ()
  (interactive)
  (call-interactively
   (if (fboundp #'display-line-numbers-mode)
       #'display-line-numbers-mode
     #'nlinum-mode)))


(bind-key "C-c d" #'delete-pair)

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

(bind-key "C-x M-!" #'find-file-path)
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
(bind-key [remap set-selective-display] #'toggle-selective-display)

(bind-key [remap move-beginning-of-line]
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
(bind-key "M-k" #'smart-kill-whole-lines)
(bind-key "M-K" #'smart-yank-whole-lines)

(bind-key "C-x C-M-t" #'transpose-regions)

(bind-key "<f9>" #'menu-bar-open)

(windmove-default-keybindings)


(defun shrink-all-windows-if-larger-than-buffer ()
  (interactive)
  (mapcar #'shrink-window-if-larger-than-buffer (window-list)))

(bind-key "C-M-r" #'isearch-query-replace isearch-mode-map)


(provide 'my-keys)
