(defun add-hooks (modes hook &optional modifier)
  (mapcar '(lambda (mode)
             (add-hook (funcall (or modifier
                                    'identity)
                                mode)
                       hook)) modes))


(add-hook 'c-mode-common-hook
          '(lambda ()
             (local-set-key (kbd "C-c o") 'ff-find-other-file)
             (local-set-key (kbd "C-c C-r") 'cc-extract)
             (local-set-key (kbd "C-c C-t") 'cc-headerize)
             (hs-minor-mode 1)))
(add-hook 'c++-mode-hook
          '(lambda ()
             (make-local-variable 'c-macro-cppflags)
             (setq c-macro-cppflags "-x c++")))




;;; lisp-hooks
(defun my-lisp-common-hook ()
  (define-key lisp-mode-map (kbd "RET") 'newline-and-indent)
  (paredit-mode 1)
  (make-local-variable 'parens-require-spaces)
  (setq parens-require-spaces t))

(add-hooks '(lisp-mode-hook
             emacs-lisp-mode-hook
             scheme-mode-hook
             lisp-interaction-mode-hook
             slime-repl-mode-hook
             geiser-repl-mode-hook)
           'my-lisp-common-hook)


(add-hook 'gud-mode-hook '(lambda () (setq tab-width 8)))


(add-hook 'prog-mode-hook '(lambda ()
                             (setq show-trailing-whitespace t)))
(add-hook 'mail-mode-hook '(lambda ()
                             (setq show-trailing-whitespace t)))

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(add-hook 'semantic-symref-results-mode-hook
          '(lambda () (toggle-read-only 1)))

(add-hook 'log-edit-mode-hook '(lambda () (ispell-change-dictionary "en")))

(add-hook 'mail-mode-hook '(lambda ()
                             (ispell-change-dictionary "pl")
                             (flyspell-mode)))


;;; LaTeX-hooks
(add-hooks '(latex-mode-hook
             LaTeX-mode-hook) '(lambda ()
                                 (setq tex-open-quote ",,")
                                 (setq TeX-open-quote ",,")
                                 (visual-line-mode 1)))

;;; htmlize-hooks
(add-hook 'htmlize-before-hook '(lambda ()
                                  (when (boundp 'rainbow-delimiters-mode)
                                    (setq htmlize--rainbow-delimiters-disabled nil)
                                    (when rainbow-delimiters-mode
                                      (rainbow-delimiters-mode 0)
                                      (setq htmlize--rainbow-delimiters-disabled t)))
                                  (setq htmlize--show-paren-disabled nil)
                                  (when show-paren-mode
                                    (show-paren-mode 0)
                                    (setq htmlize--show-paren-disabled t))))
(add-hook 'htmlize-after-hook '(lambda ()
                                 (when (boundp 'rainbow-delimiters-mode)
                                   (when htmlize--rainbow-delimiters-disabled
                                     (rainbow-delimiters-mode 1)
                                     (makunbound 'htmlize--rainbow-delimiters-disabled)))
                                 (when htmlize--show-paren-disabled
                                   (show-paren-mode 1)
                                   (makunbound 'htmlize--show-paren-disabled))))


(use-package winner
  :defer t
  :config (progn
            (define-key winner-mode-map (kbd "<XF86Back>")    'winner-undo)
            (define-key winner-mode-map (kbd "<XF86Forward>") 'winner-redo)))

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(eval-after-load "asm-mode"
  '(require 'my-asm-hooks))

(provide 'my-hooks)
