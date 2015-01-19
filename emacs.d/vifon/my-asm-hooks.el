(defun asm-colon ()
  "Insert a colon; if it follows a label, delete the label's indentation."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (skip-syntax-backward "w_")
      (skip-syntax-backward " ")
      (if (setq labelp (bolp)) (delete-horizontal-space)))
    (call-interactively 'self-insert-command)
    (when labelp
      (delete-horizontal-space))))

(defun my-asm-tab (&optional arg)
  (interactive)
  (unless (asm--in-literal)
    (delete-horizontal-space)
    (tab-to-tab-stop)))

(defun asm--in-literal ()
  (let ((beginning (save-excursion
                     (beginning-of-line)
                     (point))))
    (save-excursion
      (and (or (search-backward ";" beginning t)
               (search-backward "#" beginning t)
               (oddp (count-matches "[^\\]\"" beginning (point))))
           t))))

(font-lock-add-keywords 'asm-mode
                          `(("\\b\\(\\$[a-zA-Z0-9]*\\)\\b"
                             1 'font-lock-variable-name-face t)
                            ("\\b\\(rax\\|eax\\|ax\\|al\\|ah\\|rbx\\|ebx\\|bx\\|rcx\\|ecx\\|cx\\|rdx\\|edx\\|dx\\|rdi\\|edi\\|rsi\\|esi\\|rsp\\|esp\\|sp\\|rbp\\|ebp\\|bp\\)\\b"
                             1 'font-lock-variable-name-face t)
                            (,(concat "\\b\\("
                                      (mapconcat '(lambda (x)
                                                    (concat "r"
                                                            (number-to-string x)))
                                                 (loop for i from 8 to 15 collecting i)
                                                 "\\|")
                                      "\\)[dwb]?\\b")
                             1 'font-lock-variable-name-face t)
                            ("\\b\\(syscall\\)\\b"
                             1 'font-lock-warning-face t)))

(defun my-asm-hook ()
  (fic-mode 1)
  (set (make-local-variable 'tab-stop-list) '(8 24 40 56 72))
  (set (make-local-variable 'tab-width) 16)

  (local-set-key (kbd "TAB") '(lambda () (interactive)
                                (self-insert-command 1)
                                (my-asm-tab)))
  (local-set-key (kbd ",") '(lambda () (interactive)
                              (my-asm-tab)))
  (local-set-key (kbd "DEL") '(lambda () (interactive)
                                (if (and (= (preceding-char) #x20)
                                         (not (asm--in-literal)))
                                    (delete-horizontal-space t)
                                    (delete-backward-char 1)))))

(add-hook 'asm-mode-hook 'my-asm-hook)

(provide 'my-asm-hooks)
