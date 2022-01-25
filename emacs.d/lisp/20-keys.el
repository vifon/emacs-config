;;; -*- lexical-binding: t; -*-

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

;;; Better versions of default commands.
(bind-key [remap just-one-space] #'cycle-spacing)
(bind-key [remap upcase-word] #'upcase-dwim)
(bind-key [remap downcase-word] #'downcase-dwim)
(bind-key [remap capitalize-word] #'capitalize-dwim)
(bind-key [remap count-words-region] #'count-words)
(bind-key [remap eval-last-sexp] #'pp-eval-last-sexp)
(bind-key [remap eval-expression] #'pp-eval-expression)

(bind-key "C-x M-e" #'pp-macroexpand-last-sexp)

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
(bind-key [remap ispell-complete-word] #'completion-at-point)

(defun display-line-numbers-best ()
  (interactive)
  (call-interactively
   (if (fboundp #'display-line-numbers-mode)
       #'display-line-numbers-mode
     #'nlinum-mode)))


(bind-key "C-c d" #'delete-pair)

(add-hook 'eval-expression-minibuffer-setup-hook
          #'paredit-mode)

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

(defun vifon/toggle-selective-display (arg)
  (interactive "P")
  (if arg
      (set-selective-display arg)
    (set-selective-display (and (zerop (or selective-display 0))
                                (not (zerop (current-column)))
                                (current-column)))))
(bind-key [remap set-selective-display] #'vifon/toggle-selective-display)

(bind-key [remap move-beginning-of-line]
          (defun vifon/move-beginning-of-line (arg)
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

;;; http://endlessparentheses.com/emacs-narrow-or-widen-dwim.html
(defun narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, subtree, or defun, whichever applies
first.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning) (region-end)))
        ((derived-mode-p 'org-mode) (org-narrow-to-subtree))
        (t (narrow-to-defun))))
(bind-key "C-c l" #'narrow-or-widen-dwim)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))
(bind-key "M-Q" #'unfill-paragraph)


(bind-key "<f9>" #'menu-bar-open)

(windmove-default-keybindings)


(bind-key "C-M-r" #'isearch-query-replace isearch-mode-map)
