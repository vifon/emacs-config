;;; -*- lexical-binding: t; -*-

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode . paredit-mode)
         (eval-expression-minibuffer-setup . paredit-mode))
  :init (setq paredit-space-for-delimiter-predicates
              (list (lambda (endp delimiter) nil)))
  :bind (([remap kill-line] . vifon/paredit-kill)
         :map paredit-mode-map
         ;; Allow the global M-s keymap to be used with paredit.
         ;; Bind `paredit-splice-sexp' under this keymap instead.
         ("M-s" . nil)
         ("M-s M-s" . paredit-splice-sexp)
         ;; Do not bind C-j in modes that already override it.
         ;; Instead remap the global binding.  Fixes a minor annoyance
         ;; with `lisp-interaction-mode'.
         ("C-j" . nil)
         ([remap electric-newline-and-maybe-indent] . paredit-newline)))

(use-package package-lint
  :straight t
  :defer t)

(use-package highlight-defined
  :straight t
  :commands highlight-defined-mode)

(use-package slime
  :straight t
  :defer t
  :config (when (file-readable-p "~/quicklisp/slime-helper.el")
            (load "~/quicklisp/slime-helper")))
(use-package slime-autoloads
  :config (setq inferior-lisp-program "sbcl"))
