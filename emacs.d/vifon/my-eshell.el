(defun eshell-hooks ()
  (add-to-list 'eshell-visual-commands "vim"))

(eval-after-load "eshell"
  '(defun eshell/ccat (file)
     "Like `cat' but output with Emacs syntax highlighting."
     (with-temp-buffer
       (insert-file-contents file)
       (let ((buffer-file-name file))
         (delay-mode-hooks
           (set-auto-mode)
           (if (fboundp 'font-lock-ensure)
               (font-lock-ensure)
             (with-no-warnings
               (font-lock-fontify-buffer)))))
       (buffer-string))))

(eval-after-load "em-ls"
    '(progn
       (defun ted-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (let ((filename (buffer-substring-no-properties
                          (save-excursion
                            (goto-char (previous-single-property-change point 'help-echo))
                            (skip-chars-forward " ")
                            (point))
                          (save-excursion
                            (goto-char (next-single-property-change point 'help-echo))
                            (skip-chars-backward " ")
                            (point)))))
           (if (file-directory-p filename)
               (progn
                 (eshell/cd filename)
                 (eshell-reset))
             (find-file filename))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
         (interactive "e")
         (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
         (defvar ted-eshell-ls-keymap map))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, mouse-2: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)))

(add-hook 'eshell-mode-hook 'eshell-hooks)

(setq eshell-hist-ignoredups t)

(provide 'my-eshell)
