;; -*- lexical-binding: t; -*-

(use-package selectrum
  :ensure t
  :bind (("C-x M-r" . selectrum-repeat))
  :init (selectrum-mode 1))

(use-package selectrum-prescient
  :ensure t
  :after selectrum
  :config (selectrum-prescient-mode 1))

(use-package prescient
  :ensure t
  :defer t
  :config (progn
            (setq prescient-sort-length-enable nil)
            (prescient-persist-mode 1)))

(use-package embark
  :ensure t
  :bind (("M-O" . embark-act)
         :map minibuffer-local-map
         ("M-o"   . vifon/selectrum-embark-act)
         ("C-M-o" . vifon/selectrum-embark-act-noexit))
  :config (progn
            ;; `embark-act' seems to act on the input, not the
            ;; completion candidate, when the candidate does not start
            ;; with the input but is merely a substring.
            ;; Calling `selectrum-insert-current-candidate' beforehand
            ;; should do the trick.
            (defun vifon/selectrum-embark-act ()
              (interactive)
              (selectrum-insert-current-candidate)
              (embark-act))
            (defun vifon/selectrum-embark-act-noexit ()
              (interactive)
              (selectrum-insert-current-candidate)
              (embark-act-noexit))))

(provide 'my-completing-read)
