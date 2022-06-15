;;; -*- lexical-binding: t; -*-

(use-package cperl-mode
  :commands cperl-mode
  :mode ("\\.t\\'" . cperl-mode)
  :init (progn
          (defalias 'perl-mode 'cperl-mode)
          (setq cperl-indent-level 4
                cperl-close-paren-offset -4
                cperl-continued-statement-offset 4
                cperl-indent-parens-as-block t
                cperl-tab-always-indent t)
          (defun perl-method-call-dwim (arg)
            (interactive "P")
            (let ((inside-comment-or-string-p
                   (lambda () (nth 8 (syntax-ppss))))
                  (cursor-not-after-word-p
                   (lambda ()
                     (save-excursion
                       (backward-char)
                       (not (looking-at "[])}[:alpha:]]"))))))
              (if (or arg
                      (funcall inside-comment-or-string-p)
                      (funcall cursor-not-after-word-p))
                  (self-insert-command 1)
                (insert "->")))))
  :bind (:map cperl-mode-map
         ("." . perl-method-call-dwim)))

(use-package perltidy
  :after cperl-mode
  :bind (:map cperl-mode-map
         ("C-c C-i" . perltidy-dwim-safe)))

(use-package python
  :bind (:map python-mode-map
         ([remap yank] . vifon/python-yank))
  :config (add-hook 'python-mode-hook
                    (defun my-python-hook ()
                      (setq tab-width 4
                            python-indent 4
                            py-indent-offset 4))))

(use-package sh-script
  :mode ("/\\.env\\(?:rc\\)?\\'" . sh-mode)
  :bind (:map sh-mode-map
         ("C-c C-m" . man)))

(use-package web-mode
  :straight t
  :mode (("\\.html\\'"      . web-mode)
         ("\\.html\\.[^.]+\\'" . web-mode)
         ("\\.tt\\'"        . web-mode)
         ("\\.vue\\'"       . web-mode))
  :config (progn
            (define-derived-mode web-css-mode web-mode "WebCSS")
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)
            (setq web-mode-engines-alist
                  '(("jinja" . "\\.j2\\'")
                    ("jinja" . "\\.html\\'")))
            ;; Fix Jinja2 autopairing; was producing: "{{  }}}}".
            (setq web-mode-enable-auto-pairing nil)))

(use-package cmake-mode
  :straight t
  :defer t)

(use-package haskell-mode
  :straight t
  :defer t
  :config (progn
            (setq haskell-program-name "cabal repl")
            (add-hook 'haskell-mode-hook #'interactive-haskell-mode)))

(use-package js-mode
  :defer t
  :init (setq-default js-indent-level 2))
(use-package typescript-mode
  :straight t
  :defer t
  :init (setq-default typescript-indent-level 2))

(use-package fish-mode
  :straight t
  :mode ("\\`/tmp/tmp\\.[a-zA-Z0-9]+\\.fish\\'" . fish-mode)
  :bind (:map fish-mode-map
         ("C-c C-m" . man)))


(use-package rust-mode :straight t :defer t)

(use-package go-mode
  :straight t
  :defer t
  :config (progn
            (setq gofmt-args '("-s"))
            (vifon/gofmt-global-mode 1)))

(use-package lua-mode :straight t :defer t)

(use-package nix-mode :straight t :defer t)

(use-package ess
  :straight t
  :defer t
  :config (setq ess-keep-dump-files nil
                ess-delete-dump-files t
                ess-history-file nil))
