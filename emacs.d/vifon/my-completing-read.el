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
  :config (prescient-persist-mode 1))

(use-package orderless
  :ensure t
  :after selectrum
  :config (setq orderless-matching-styles '(orderless-regexp
                                            orderless-initialism
                                            orderless-prefixes)
                selectrum-refine-candidates-function #'orderless-filter
                selectrum-highlight-candidates-function #'orderless-highlight-matches))

(use-package embark
  :ensure t
  :bind (("C-c o" . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act)
         ("C-M-o" . embark-act-noexit)))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package marginalia
  :ensure t
  :after selectrum
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once selectrum is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :ensure t
  :bind (("M-s f" . consult-line)
         ("M-g g" . consult-line)
         ("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line))
  :config (setq consult-config `((consult-buffer :preview-key ,(kbd "TAB")))))

(provide 'my-completing-read)
