;;; -*- lexical-binding: t; -*-

;;; TODO(28.2?): Migrate to the built-in version of project.el
(use-package project
  :straight t
  :defer t
  :config (setq project-switch-commands #'project-dired))

(use-package hideshow-org
  :straight t
  :bind ("M-RET" . hs-toggle-hiding)
  :commands hs-org/minor-mode)

(use-package folding
  :straight t
  :commands folding-mode
  :config (folding-add-to-marks-list 'cperl-mode
                                     "=over" "=back"))

(use-package auctex
  :straight t
  :defer t
  :init (progn
          (setq preview-scale-function 2.0)
          (defun my-auctex-build-pdf ()
            (interactive)
            (TeX-command "LaTeX" 'TeX-master-file))))

(use-package transpose-frame
  :straight t
  :bind (("C-x 4 t" . transpose-frame)
         ("C-x 4 i" . flop-frame)
         ("C-x 4 I" . flip-frame)))

(use-package treemacs
  :straight t
  :bind (("C-c b" . treemacs)
         :map treemacs-mode-map
         ("j" . treemacs-next-line)
         ("k" . treemacs-previous-line)
         ("W" . treemacs-switch-workspace)))

(use-package goto-last-change
  :straight t
  :demand t
  :bind ("C-x C-\\" . goto-last-change))

(use-package symbol-overlay
  :straight t
  :bind (("M-p" . symbol-overlay-jump-prev)
         ("M-n" . symbol-overlay-jump-next)
         ("C-;" . symbol-overlay-put)
         :map symbol-overlay-map
         ("b" . symbol-overlay-switch-backward)
         ("f" . symbol-overlay-switch-forward)))

(use-package highlight-indent-guides
  :straight t
  :commands highlight-indent-guides-mode
  :config (setq highlight-indent-guides-method 'column)
  :init (define-globalized-minor-mode global-highlight-indent-guides-mode highlight-indent-guides-mode
          (lambda ()
            (when (derived-mode-p 'prog-mode)
              (highlight-indent-guides-mode 1)))))

(use-package presentation
  :straight t
  :commands presentation-mode)

(use-package visual-regexp
  :straight t
  :defer t
  :bind (([remap query-replace-regexp] . vr/query-replace)))
(use-package visual-regexp-steroids
  :straight t
  :after visual-regexp)

(use-package volatile-highlights
  :straight t
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode 1))

(use-package beacon
  :straight t
  :diminish beacon-mode
  :config (beacon-mode 1))

(use-package phi-search
  :straight t
  :commands (phi-search phi-search-backward))
(use-package multiple-cursors
  :straight t
  :demand t                             ;C-v/M-v don't work otherwise
  :bind (("C-<"         . mc/mark-previous-like-this)
         ("C->"         . mc/mark-more-like-this-extended)
         ("C-*"         . mc/mark-all-like-this-dwim)
         ("C-M-;"       . mc/mark-all-like-this-dwim)
         ("M-<mouse-1>" . mc/add-cursor-on-click)
         ("M-<down-mouse-1>" . nil)
         :map mc/keymap
         ("C-s" . phi-search)
         ("C-r" . phi-search-backward)))

(use-package kmacro-x
  :straight t
  :bind ("C-M-:" . kmacro-x-mc)
  :custom (kmacro-x-atomic-undo-mode t)
  :diminish kmacro-x-atomic-undo-mode)

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("M-S-<SPC>" . er/expand-region)))

(use-package flycheck
  :straight t
  :defer t)

(use-package avy
  :straight t
  :bind ("C-c j" . avy-goto-char-timer))

(use-package deft
  :straight t
  :bind (("<f5>" . deft)
         :map deft-mode-map
         ("<f5>" . deft-index))
  :init (defun deft-index ()
          (interactive)
          (dired deft-directory))
  :config (progn
            (setq deft-auto-save-interval 0
                  deft-default-extension "org"
                  deft-new-file-format "%Y%m%dT%H%M%S--new"
                  deft-use-filter-string-for-filename nil
                  deft-file-naming-rules '((noslash . "-")
                                           (nospace . "-")
                                           (case-fn . downcase)))
            (define-key deft-mode-map (kbd "C-c C-n") #'zettel2-create-note)))

(use-package zettel2
  :straight (:host github :repo "vifon/zettel2"
             :files (:defaults "graph.pl"))
  :mode (("/\\.deft/[^/]+\\.org\\'" . zettel2-mode)
         ("/zettels?/[^/]+\\.org\\'" . zettel2-mode))
  :config (progn
            (require 'zettel2-link)
            (setq zettel2-graph-format "png")))

(use-package markdown-mode
  :straight t
  :defer t
  :config (add-hook 'markdown-mode-hook
                    (defun my-markdown-mode-hook ()
                      (add-to-list (make-local-variable 'electric-pair-pairs)
                                   '(?` . ?`)))))

(use-package ansible
  :straight t
  :defer t
  :init (add-hook 'yaml-mode-hook
                  (defun ansible-maybe (&optional arg)
                    (interactive "P")
                    (require 'yasnippet) ;Load ansible-specific snippets.
                    (when (or (string-match-p "/roles/.*\\.yml\\'" (buffer-file-name))
                              (string-match-p "/main\\.yml\\'" (buffer-file-name)))
                      (ansible arg)))))

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.sls\\'" . yaml-mode)       ;Saltstack files
  )

(use-package dumb-jump
  :straight t
  :hook ((cperl-mode    . dumb-jump-activate)
         (c-mode-common . dumb-jump-activate)
         (sh-mode       . dumb-jump-activate))
  :init (defun dumb-jump-activate ()
          (interactive)
          (add-hook 'xref-backend-functions
                    #'dumb-jump-xref-activate
                    nil t)))

(use-package vlf
  :straight t
  :init (require 'vlf-setup))

(use-package lsp-mode
  :straight t
  :defer t
  :hook ((python-mode . lsp-deferred)
         (go-mode     . lsp-deferred)
         (rust-mode   . lsp-deferred))
  :init (setq lsp-keymap-prefix "C-c k"
              lsp-rust-server 'rust-analyzer
              lsp-signature-auto-activate nil
              lsp-headerline-breadcrumb-enable nil))

(use-package lsp-ui
  :straight t
  :after lsp-mode
  :bind (:map lsp-ui-mode-map
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :init (setq lsp-ui-doc-enable nil))

(use-package rg
  :straight t
  :commands rg
  :bind ("M-s ," . rg-dwim))

(use-package ledger-mode
  :straight t
  :defer t
  :bind (:map ledger-mode-map
         ("<C-tab>" . nil)
         ("C-M-i" . completion-at-point))
  :config (progn
            (setq ledger-clear-whole-transactions t
                  ledger-highlight-xact-under-point nil
                  ledger-default-date-format ledger-iso-date-format
                  ledger-reconcile-default-commodity "PLN"
                  ledger-post-amount-alignment-at :end)
            (defun vifon/delete-blank-lines (&rest ignored)
              "Same as `delete-blank-lines' but accept (and
ignore) any passed arguments to work as an advice."
              (let ((point (point)))
                (goto-char (point-max))
                (delete-blank-lines)
                (goto-char point)))
            (advice-add #'ledger-add-transaction :after
                        #'vifon/delete-blank-lines)
            (advice-add #'ledger-fully-complete-xact :after
                        #'vifon/delete-blank-lines)))

(use-package scratch-mode
  :straight (:host github :repo "vifon/scratch-mode")
  :bind (("C-c s" . scratch-reset)
         :map scratch-mode-map
         ("," . vifon/theme-light)
         ("." . vifon/theme-dark)
         ("/" . vifon/theme-dwim)
         ("b" . switch-to-buffer)
         ("S" . vifon/make-scratch-dir)
         ("P" . straight-use-package)
         ("R" . recentf-edit-list))
  :init (setq initial-major-mode 'scratch-mode)
  :config (progn
            (bind-key "c" (lambda ()
                            (interactive)
                            (unless (bound-and-true-p chronos--buffer)
                              (require 'chronos)
                              (chronos-initialize))
                            (switch-to-buffer chronos--buffer))
                      scratch-mode-map)
            (dolist (key-spec '(("c" . "chronos")
                                ("S" . "make-scratch-dir")
                                "R"))
              (add-to-list 'scratch-mode-key-hints key-spec 'append))
            (vifon/add-to-list-after
             'scratch-mode-key-hints "e"
             (cons "("
                   (lambda (k)
                     (format "%s + %s" #'lisp-interaction-mode k))))
            (bind-key "l" (lambda ()
                            (interactive)
                            (find-file
                             (expand-file-name "lisp/"
                                               user-emacs-directory)))
                      scratch-mode-map)
            (vifon/add-to-list-after
             'scratch-mode-key-hints
             '("I" . "~/.emacs.d/early-init.el")
             '("l" . "~/.emacs.d/lisp/"))))

(use-package circe
  :straight t
  :defer t
  :if (file-directory-p "~/.password-store/emacs/circe")
  :bind (:map lui-mode-map
         ("C-c C-w" . lui-track-bar-move)
         :map circe-mode-map
         ("C-c C-q". bury-buffer))
  :config (progn
            (setq circe-server-buffer-name "Circe:{network}")
            (let ((servers (eval (read (shell-command-to-string
                                        "pass emacs/circe/servers.el")))))
              (setq circe-network-options servers))

            (setq circe-reduce-lurker-spam t)

            (setq circe-use-cycle-completion t)

            (setq circe-format-server-topic
                  (replace-regexp-in-string "{new-topic}"
                                            "{topic-diff}"
                                            circe-format-server-topic))

            (enable-circe-color-nicks)
            (setq circe-color-nicks-everywhere t)

            (enable-lui-track-bar)

            (bind-key "C-c C-o"
                      (lambda ()
                        (interactive)
                        (ffap-next-url t))
                      lui-mode-map)))

(use-package nlinum
  :straight t
  :defer t
  :if (not (fboundp 'display-line-numbers-mode)))

(use-package hyperlist-mode
  :straight t
  :mode ("\\.hl\\'" . hyperlist-mode))

(use-package aggressive-indent :straight t :defer t)
(use-package color-identifiers-mode :straight t :defer t)
(use-package dockerfile-mode :straight t :defer t)
(use-package dpaste :straight t :defer t)
(use-package impatient-mode :straight t :defer t)
(use-package rainbow-mode :straight t :defer t)
(use-package restclient :straight t :defer t)
(use-package web-beautify :straight t :defer t)
(use-package wgrep :straight t :defer t)
(use-package ripgrep :straight t :defer t)
(use-package orgalist :straight t :defer t)
(use-package chronos :straight t :defer t)
(use-package writeroom-mode :straight t :defer t)
(use-package olivetti :straight t :defer t)
