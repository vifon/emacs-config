(defun my-lineup (beg end &optional step minshift)
  (interactive "r\ni\nP")
  (let (search col tcol seen b)
    (save-excursion
      (goto-char end)
      (end-of-line)
      (setq end (point-marker))
      (goto-char beg)
      (skip-chars-forward " \t\f")
      (setq beg (point-marker))
      (indent-region beg end nil)
      (goto-char beg)
      (setq col (current-column))
      (if (looking-at "[a-zA-Z0-9_]")
          (if (looking-at "\\<[a-zA-Z0-9_]+\\>")
              (setq search
                    (concat "\\<"
                            (regexp-quote
                             (buffer-substring (match-beginning 0)
                                               (match-end 0))) "\\>"))
              (error "Cannot line up in a middle of the word"))
          (if (looking-at "$")
              (error "Cannot line up end of line"))
          (setq search (regexp-quote (char-to-string (following-char)))))
      (setq step (or step tab-width))
      (or minshift (setq minshift 0))
      (while (progn
               (beginning-of-line 2)
               (and (< (point) end)
                    (re-search-forward search end t)
                    (goto-char (match-beginning 0))))
        (setq tcol (current-column) seen t)
        (if (> tcol col) (setq col tcol)))
      (or seen
          (error "The construction to line up occurred only once"))
      (goto-char beg)
      (setq col (+ col minshift))
      (if (/= (% col step) 0) (setq step (* step (1+ (/ col step)))))
      (while
          (progn
            (my-make-indent col)
            (beginning-of-line 2)
            (and (< (point) end)
                 (re-search-forward search end t)
                 (goto-char (match-beginning 0))))))))


(defun my-make-indent (column &optional minimum keep)
  "Makes indent of the current line the requested amount.
Unless KEEP, removes the old indentation.  Works around a bug in ancient
versions of Emacs."
  (let ((prop (get-text-property (point) 'syntax-type)))
    (or keep
        (delete-horizontal-space))
    (indent-to column minimum)
    ;; In old versions (e.g., 19.33) `indent-to' would not inherit properties
    (and prop
         (> (current-column) 0)
         (save-excursion
           (beginning-of-line)
           (or (get-text-property (point) 'syntax-type)
               (and (looking-at "\\=[ \t]")
                    (put-text-property (point) (match-end 0)
                                       'syntax-type prop)))))))

(global-set-key (kbd "M-C-|") 'my-lineup)

(provide 'lineup)
