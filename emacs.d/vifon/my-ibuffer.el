(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)
         :map ibuffer-mode-map
         ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root))
  :config (progn
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
                                          " " filename)))))

(use-package ibuffer-vc
  :ensure t
  :commands ibuffer-vc-set-filter-groups-by-vc-root)

(provide 'my-ibuffer)
