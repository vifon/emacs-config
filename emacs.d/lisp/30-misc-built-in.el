;;; -*- lexical-binding: t; -*-

(use-package ibuffer
  :bind (:map ibuffer-mode-map
         ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
         ("/ T" . ibuffer-tramp-set-filter-groups-by-tramp-connection))
  :init (bind-key "C-x C-b"
                  (if (fboundp 'ibuffer-jump)
                      #'ibuffer-jump
                    #'ibuffer))
  :config (progn
            (add-hook 'ibuffer-mode-hook
                      (lambda ()
                        (setq ibuffer-sorting-mode 'filename/process)))
            (setq ibuffer-show-empty-filter-groups nil)
            (setq ibuffer-expert t)
            (setq ibuffer-formats
                  `((mark modified read-only
                          ,@(when (version<= "26.1" emacs-version)
                              '(locked))
                          " "
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
  :straight t
  :commands ibuffer-vc-set-filter-groups-by-vc-root)

(use-package ibuffer-tramp
  :straight t
  :commands ibuffer-tramp-set-filter-groups-by-tramp-connection)

(use-package recentf
  :init (recentf-mode 1)
  :config (setq recentf-exclude '("\\`/media/"
                                  "\\`/run/media/"
                                  "/COMMIT_EDITMSG\\'")
                recentf-max-menu-items 10000
                recentf-max-saved-items 1000))

(use-package paren
  :config (progn
            (setq show-paren-delay 0
                  show-paren-when-point-inside-paren t
                  show-paren-when-point-in-periphery t)
            (show-paren-mode 1)))

(use-package semantic/decorate/mode
  :after semantic
  :config (setq-default semantic-decoration-styles
                        '(("semantic-decoration-on-includes" . t))))

(use-package semantic
  :defer t
  :config (setq semantic-default-submodes
                '(global-semantic-idle-scheduler-mode
                  global-semanticdb-minor-mode
                  global-semantic-decoration-mode
                  global-semantic-stickyfunc-mode)))

(use-package winner
  :bind ("C-c z" . winner-undo)
  :init (winner-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
                uniquify-strip-common-suffix t))

(use-package image-mode
  :defer t
  :config (progn
            (bind-key "k"
                      (lambda (arg)
                        (interactive "P")
                        (if arg
                            (kill-buffer-and-window)
                          (kill-buffer)))
                      image-mode-map)
            (bind-key "K" #'kill-buffer-and-window
                      image-mode-map)))

(use-package cua-base
  :defer t
  :init (progn
          ;; Disabling `cua-global-mark-keep-visible' improves
          ;; performance when typing characters with global-mark
          ;; enabled.
          (setq cua-global-mark-keep-visible nil)

          ;; Do not enable more cua keys than necessary.
          (setq cua-enable-cua-keys nil)

          ;; Using `cua-selection-mode' alone is not enough. It still
          ;; binds C-RET and overrides this keybinding for instance in
          ;; org-mode. I'd rather keep using my hack.
          (bind-key "C-S-SPC"
                    (defun cualess-global-mark ()
                      (interactive)
                      (if (version<= "24.4" emacs-version)
                          (cua-selection-mode 1)
                        (cua-mode 1))
                      (call-interactively #'cua-toggle-global-mark)))
          (advice-add #'cua--deactivate-global-mark :after
                      (lambda (ret-value)
                        (cua-mode 0)
                        ret-value))))

(use-package midnight
  :defer 13
  :config (midnight-mode 1))
