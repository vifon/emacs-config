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
         ("M-o"   . embark-act)
         ("C-M-o" . embark-act-noexit))
  :config (progn
            ;; Source: https://github.com/raxod502/selectrum/wiki/Additional-Configuration#minibuffer-actions-with-embark
            (progn
              (require 'selectrum)
              (add-hook 'embark-target-finders #'selectrum-get-current-candidate)

              (add-hook 'embark-candidate-collectors
                        (defun embark-selectrum-candidates+ ()
                          (when selectrum-active-p
                            (selectrum-get-current-candidates
                             ;; Pass relative file names for dired.
                             minibuffer-completing-file-name))))

              ;; No unnecessary computation delay after injection.
              (add-hook 'embark-setup-hook #'selectrum-set-selected-candidate)

              (add-hook 'embark-input-getters
                        (defun embark-selectrum-input-getter+ ()
                          (when selectrum-active-p
                            (let ((input (selectrum-get-current-input)))
                              (if minibuffer-completing-file-name
                                  ;; Only get the input used for matching.
                                  (file-name-nondirectory input)
                                input))))))))

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
  :bind (("M-s f" . consult-line-symbol-at-point)
         ("C-x C-r" . consult-recent-file)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)))

(use-package consult-selectrum
  :ensure t
  :after consult)

(provide 'my-completing-read)
