;;; -*- lexical-binding: t; -*-

(use-package yasnippet
  :straight t
  :defer 7
  :diminish yas-minor-mode
  :commands yas-global-mode
  :mode ("/yasnippet/snippets/" . snippet-mode)
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package auto-yasnippet
  :straight t
  :bind (("C-c Y" . aya-create)
         ("C-c y" . aya-expand-with-indent))
  :init (defun aya-expand-with-indent (arg)
          (interactive "P")
          (aya-expand)
          (unless arg
            (indent-for-tab-command))))

(use-package tiny
  :straight t
  :bind ("C-:" . tiny-expand))

(use-package emmet-mode
  :straight t
  :hook (web-mode
         html-mode
         css-mode)
  :commands emmet-mode
  :config (progn
            (setq emmet-self-closing-tag-style " /")
            (add-to-list 'emmet-css-major-modes 'web-css-mode)))

(use-package legalese
  :straight t
  :commands legalese
  :init (defun legalese-box (ask)
          (interactive "P")
          (let ((comment-style 'box))
            (legalese ask))))
