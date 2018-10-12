(use-package el-patch
  :ensure t
  :config (setq el-patch-use-aggressive-defvar t))

(el-patch-feature ledger-complete)
(with-eval-after-load 'ledger-complete
  (el-patch-defun ledger-fully-complete-xact ()
    "Completes a transaction if there is another matching payee in the buffer.
Does not use ledger xact"
    (interactive)
    (let* ((name (ledger-trim-trailing-whitespace (caar (ledger-parse-arguments))))
           (rest-of-name name)
           xacts)
      (save-excursion
        (when (eq 'transaction (ledger-thing-at-point))
          (delete-region (point) (+ (length name) (point)))
          ;; Search backward for a matching payee
          (el-patch-add (require 'cl-extra))
          (when (el-patch-wrap 1 1
                  (cl-some (el-patch-wrap 2
                             (lambda (x)
                               (re-search-backward
                                (concat (el-patch-concat
                                          "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\("
                                          (el-patch-remove ".*"))
                                        (el-patch-add (car x))
                                        (regexp-quote name)
                                        (el-patch-swap ".*\\)"
                                                       (cdr x))
                                        (el-patch-add "\\)"))
                                nil t)))
                           '(("" . "$") (".*" . ".*"))))
            (setq rest-of-name (match-string 3))
            ;; Start copying the postings
            (forward-line)
            (while (looking-at ledger-account-any-status-regex)
              (setq xacts (cons (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position))
                                xacts))
              (forward-line))
            (setq xacts (nreverse xacts)))))
      ;; Insert rest-of-name and the postings
      (when xacts
        (save-excursion
          (insert rest-of-name ?\n)
          (while xacts
            (insert (car xacts) ?\n)
            (setq xacts (cdr xacts))))
        (forward-line)
        (goto-char (line-end-position))
        (if (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
            (goto-char (match-end 0)))))))

(provide 'my-el-patch)
