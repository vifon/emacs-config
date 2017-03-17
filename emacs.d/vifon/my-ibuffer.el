(setq ibuffer-saved-filter-groups '(("vifon"
                                     ("terminals" (or (mode . eshell-mode)
                                                      (mode . term-mode)))
                                     ("*emacs*" (or (name . "^\\*.*\\*$")
                                                    (name . "^\\*.*\\*<[0-9]+>$")))
                                     ("emacs-config" (or (filename . "\\.emacs\\.d")
                                                         (filename . "emacs-config")
                                                         (filename . "\\.emacs")
                                                         (mode . emacs-lisp-mode)))
                                     ("Documents" (or (mode . doc-view-mode)
                                                      (mode . image-mode)))
                                     ("dired" (or (mode . dired-mode)
                                                  (mode . wdired-mode)))
                                     ("org" (mode . org-mode)))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (setq ibuffer-sorting-mode 'filename/process)))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
(setq ibuffer-formats '((mark modified read-only " "
                              (name 32 32 :left :elide)
                              " "
                              (size 9 -1 :right)
                              " "
                              (mode 16 64 :left :elide)
                              " " filename-and-process)
                        (mark " "
                              (name 32 -1)
                              " " filename)))

(global-set-key (kbd "C-x C-b") #'ibuffer)

(use-package ibuffer
  :config (progn
            (use-package ibuffer-vc)
            (define-key ibuffer-mode-map
              (kbd "/ V") #'ibuffer-vc-set-filter-groups-by-vc-root)))

(provide 'my-ibuffer)
