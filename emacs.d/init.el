;;; -*- lexical-binding: t; -*-
(require 'early-init "~/.emacs.d/early-init.el")

(add-to-list 'load-path "~/.emacs.d/vifon")
(add-to-list 'load-path "~/.emacs.d/modules")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(load "my-el-patch")
(load "my-hooks")
(load "my-skeletons")
(load "my-mode-line")
(load "my-fun")
(load "my-keys")
(load "my-transient")
(load "my-completing-read")
(load "my-org")
(load "my-eshell")
(load "my-settings")
(load "my-spellcheck")
(when (getenv "START_EXWM")
  (load "my-exwm")
  (setenv "START_EXWM"))

(load "pastes-from-web")


(use-package project
  :straight t
  :defer t)

(use-package s :straight t :defer t)
(use-package paredit
  :straight t
  :diminish "[()]"
  :commands paredit-kill
  :hook (emacs-lisp-mode . paredit-mode)
  :init (progn
          (setq paredit-space-for-delimiter-predicates
                '((lambda (endp delimiter) nil)))
          (defun paredit-kill-maybe (arg)
            (interactive "P")
            (if (consp arg)
                (paredit-kill)
              (kill-line arg))))
  :bind (([remap kill-line] . paredit-kill-maybe)
         :map paredit-mode-map
         ("M-s" . nil)
         ("M-s M-s" . paredit-splice-sexp)))

(use-package hideshow-org
  :straight t
  :bind ("M-RET" . hs-toggle-hiding)
  :commands hs-org/minor-mode)

(use-package folding
  :straight t
  :commands folding-mode
  :config (folding-add-to-marks-list 'cperl-mode
                                     "=over" "=back"))

(use-package dired
  :bind (:map dired-mode-map
         ("z" . dired-subtree-toggle)
         ("TAB" . vifon/dired-transient)
         ("K" . vifon/dired-subdir-toggle)
         ("* C" . vifon/dired-change-marks*)
         ("E" . vifon/dired-dragon))
  :config (progn
            (setq dired-dwim-target nil
                  dired-free-space-args "-Pkh"
                  dired-ls-F-marks-symlinks t
                  dired-isearch-filenames 'dwim
                  dired-omit-files "^\\.?#\\|^\\.[^\\.]\\|^\\.\\.."
                  wdired-allow-to-change-permissions t
                  image-dired-external-viewer "sxiv")

            (setq dired-listing-switches "-alh --group-directories-first -v")

            (defun vifon/dired-subdir-toggle ()
              (interactive)
              (if (equal (dired-current-directory)
                         (expand-file-name default-directory))
                  (call-interactively #'dired-maybe-insert-subdir)
                (dired-kill-subdir)
                (pop-to-mark-command)))

            (defun vifon/dired-change-marks* (&optional new)
              (interactive
               (let* ((cursor-in-echo-area t)
                      (new (progn (message "Change * marks to (new mark): ")
                                  (read-char))))
                 (list new)))
              (dired-change-marks ?* new))

            (defun vifon/dired-dragon (&optional single)
              (interactive "P")
              (dired-do-async-shell-command (if single
                                                "dragon -x *"
                                              "dragon -a -x *")
                                            nil
                                            (dired-get-marked-files)))))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("h" . dired-jump))
  :init (setq dired-x-hands-off-my-keys t))

(use-package dired-subtree
  :straight t
  :after dired
  :commands dired-subtree-toggle)

(use-package dired-filter
  :straight t
  :after dired
  :config (setq-default dired-filter-stack '()))

(use-package dired-rifle
  :straight t
  :after dired)

(use-package dired-collapse
  :straight t
  :after dired)

(use-package dired-async
  :straight async
  :after dired
  :commands dired-async-mode
  :config (setq dired-async-message-function
                (lambda (text face &rest args)
                  (call-process "notify-send" nil 0 nil
                                "Emacs: dired-async"
                                (apply #'format text args))
                  (apply #'dired-async-mode-line-message text face args))))

(use-package ibuffer
  :bind (:map ibuffer-mode-map
         ("/ V" . ibuffer-vc-set-filter-groups-by-vc-root)
         ("/ T" . ibuffer-tramp-set-filter-groups-by-tramp-connection))
  :init (bind-key "C-x C-b"
                  (if (fboundp #'ibuffer-jump)
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
  :config (recentf-mode 1))

(use-package yasnippet
  :straight t
  :defer 7
  :diminish yas-minor-mode
  :commands yas-global-mode
  :mode ("emacs\\.d/snippets/" . snippet-mode)
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode 1))

(use-package yasnippet-snippets
  :straight t
  :after yasnippet)

(use-package auto-yasnippet
  :straight t
  :bind (("C-c Y" . aya-create)
         ("C-c y" . aya-expand-with-indent))
  :config (defun aya-expand-with-indent (arg)
            (interactive "P")
            (aya-expand)
            (unless arg
              (indent-for-tab-command))))

(use-package tiny
  :straight t
  :bind ("C-:" . tiny-expand))

(use-package auctex
  :straight t
  :defer t
  :init (setq preview-scale-function 2.0)
  :config (defun my-auctex-build-pdf ()
            (interactive)
            (TeX-command "LaTeX" 'TeX-master-file)))

(use-package web-mode
  :straight t
  :mode (("\\.html\\'"      . web-mode)
         ("\\.html\\.[^.]+\\'" . web-mode)
         ("\\.tt\\'"        . web-mode)
         ("\\.vue\\'"       . web-mode)
         ("\\.css\\'"       . web-css-mode)
         ("\\.scss\\'"      . web-css-mode))
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

(use-package doom-modeline
  :straight t
  :init (progn
          (setq doom-modeline-env-version nil
                doom-modeline-icon nil
                doom-modeline-minor-modes t)
          (doom-modeline-mode 1)))

(use-package transpose-frame
  :straight t
  :bind (("C-x 4 t" . transpose-frame)
         ("C-x 4 i" . flop-frame)
         ("C-x 4 I" . flip-frame)))

(use-package treemacs
  :straight t
  :bind (("C-c b" . treemacs)
         ("C-c B" . treemacs-switch-workspace)
         :map treemacs-mode-map
         ("j" . treemacs-next-line)
         ("k" . treemacs-previous-line)
         ("W" . treemacs-switch-workspace)))

(use-package magit
  :straight t
  :defer t
  :config (progn
            (setq magit-diff-refine-hunk t
                  magit-status-goto-file-position t)
            (setq magit-repository-directories
                  '(("~/configs/" . 1)
                    ("~/projects/" . 2))
                  magit-repolist-columns
                  '(("Name" 25 magit-repolist-column-ident ())
                    ("Version" 25 magit-repolist-column-version ())
                    ("D" 1 magit-repolist-column-dirty ())
                    ("B<U" 3 magit-repolist-column-unpulled-from-upstream
                     ((:right-align t)
                      (:help-echo "Upstream changes not in branch")))
                    ("B>U" 3 magit-repolist-column-unpushed-to-upstream
                     ((:right-align t)
                      (:help-echo "Local changes not in upstream")))
                    ("Path" 99 magit-repolist-column-path ())))
            (dolist (keymap (list magit-status-mode-map
                                  magit-log-mode-map
                                  magit-reflog-mode-map
                                  magit-refs-mode-map
                                  magit-diff-mode-map))
              (let* ((oldkey (kbd "C-<tab>"))
                     (newkey (kbd "M-<tab>"))
                     (oldcmd (lookup-key keymap oldkey)))
                (when oldcmd
                  (define-key keymap newkey oldcmd)
                  (define-key keymap oldkey nil))))
            (transient-append-suffix 'magit-log "-f"
              '("-1" "First parent" "--first-parent"))
            (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package magit-todos :straight t :defer t)

(use-package forge
  :straight t
  :after magit)

(use-package git-commit
  :straight t
  :mode ("/COMMIT_EDITMSG\\'" . git-commit-mode)
  :bind (:map git-commit-mode-map
         ("C-c C-l" . magit-log))
  :config (add-hook 'git-commit-mode-hook (defun turn-on-flyspell ()
                                            (flyspell-mode 1))))

(use-package git-messenger
  :straight t
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package git-timemachine
  :straight t
  :bind ("C-x v t" . git-timemachine))

(use-package git-link
  :straight t
  :bind ("C-x v w" . git-link)
  :config (setq git-link-default-branch "master"))

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

(use-package indent-guide
  :straight t
  :defer t
  :init (add-hook 'LaTeX-mode-hook (lambda ()
                                     (indent-guide-mode 1)))
  :config (setq indent-guide-delay nil))

(use-package writeroom-mode
  :straight t
  :defer t)

(use-package olivetti
  :straight t
  :defer t)

(use-package presentation
  :straight t
  :commands presentation-mode)

(use-package highlight-defined
  :straight t
  :commands highlight-defined-mode)

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

(use-package expand-region
  :straight t
  :bind (("C-=" . er/expand-region)
         ("M-S-<SPC>" . er/expand-region)))

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

(use-package flycheck
  :straight t
  :defer t
  :init (progn
          (add-hook 'c-mode-hook (lambda ()
                                   (setq flycheck-clang-language-standard "c14")))
          (add-hook 'c++-mode-hook (lambda ()
                                     (setq flycheck-clang-language-standard "c++14")))))

(use-package cmake-mode
  :straight t
  :defer t)

(use-package avy
  :straight t
  :bind ("C-c j" . avy-goto-char-timer))

(use-package win-switch
  :straight t
  :bind ("C-x o" . win-switch-dispatch)
  :config (setq win-switch-window-threshold 2))

(use-package winner
  :bind ("C-c z" . winner-undo)
  :init (winner-mode 1))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
                uniquify-strip-common-suffix t))

(use-package deft
  :straight t
  :bind (("<f5>" . deft)
         :map deft-mode-map
         ("<f5>" . deft-index))
  :init (setq deft-auto-save-interval 0)
  :config (progn
            (setq deft-default-extension "org"
                  deft-use-filter-string-for-filename t
                  deft-file-naming-rules '((noslash . "-")
                                           (nospace . "-")
                                           (case-fn . downcase)))
            (defun deft-index ()
              (interactive)
              (deft-find-file "index.org"))))

(use-package zettel-mode
  :straight (:host github :repo "vifon/zettel-mode")
  :mode (("/\\.deft/[^/]+\\.org\\'" . zettel-mode)
         ("/zettels?/[^/]+\\.org\\'" . zettel-mode)))

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
(use-package ansible-doc :straight t :defer t)

(use-package yaml-mode
  :straight t
  :defer t
  :mode ("\\.sls\\'" . yaml-mode)       ;Saltstack files
)

(use-package dumb-jump
  :straight t
  :hook ((cperl-mode c-mode-common) . dumb-jump-activate)
  :init (defun dumb-jump-activate ()
          (interactive)
          (add-hook 'xref-backend-functions
                    #'dumb-jump-xref-activate
                    nil t)))

(use-package vlf
  :straight t
  :init (require 'vlf-setup))

(use-package haskell-mode
  :straight t
  :defer t
  :config (progn
            (setq haskell-program-name "cabal repl")
            (add-hook 'haskell-mode-hook #'interactive-haskell-mode)))

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
  :defer t
  :config (progn
            (add-hook 'python-mode-hook
                      (defun my-python-hook ()
                        (setq tab-width 4
                              python-indent 4
                              py-indent-offset 4)))))

(use-package lsp-mode
  :straight t
  :defer t
  :hook ((python-mode . lsp-deferred)
         (go-mode     . lsp-deferred)
         (rust-mode   . lsp-deferred)
         (js-mode     . lsp-deferred)
         (typescript-mode . lsp-deferred))
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

(use-package js-mode
  :defer t
  :init (setq-default js-indent-level 2))
(use-package typescript-mode
  :straight t
  :defer t
  :init (setq-default typescript-indent-level 2))

;;; For the additional project.el functionality.
(use-package magit-extras
  :after project)

(use-package rg
  :straight t
  :commands rg
  :bind ("M-s ," . rg-dwim))

(use-package sane-term
  :straight t
  :init (setq sane-term-shell-command (getenv "SHELL"))
  :config (add-hook 'term-mode-hook
                    (lambda ()
                      (yas-minor-mode -1)))
  :bind ("C-x T" . sane-term))

(use-package diff-hl
  :straight t
  :defer 2
  :bind ("C-x v \\" . diff-hl-amend-mode)
  :config (progn
            (global-diff-hl-mode 1)))

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

(use-package sh-script
  :bind (:map sh-mode-map
         ("C-c C-m" . man)))

(use-package fish-mode
  :straight t
  :bind (:map fish-mode-map
         ("C-c C-m" . man)))

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
         ("b" . consult-buffer)
         ("S" . scratch-dir))
  :init (setq initial-major-mode 'scratch-mode)
  :config (progn
            (bind-key "c" (lambda ()
                            (interactive)
                            (unless (bound-and-true-p chronos--buffer)
                              (require 'chronos)
                              (chronos-initialize))
                            (switch-to-buffer chronos--buffer))
                      scratch-mode-map)
            (add-to-list 'scratch-mode-key-hints (cons "c" "chronos") 'append)
            (add-to-list 'scratch-mode-key-hints "S" 'append)
            (vifon/add-to-list-after
             'scratch-mode-key-hints "e"
             (cons "("
                   (lambda (k)
                     (format "%s + %s" #'lisp-interaction-mode k))))))

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

(use-package notmuch
  :straight t
  :if (and (file-directory-p "~/Mail/.notmuch")
           (file-regular-p   "~/.emacs.d/secret/notmuch-fcc"))
  :bind (("<f7>" . notmuch))
  :config (progn
            (setq notmuch-fcc-dirs
                  (let ((tags "-new -unread -inbox +sent"))
                    (mapcar
                     (lambda (rule)
                       (let ((account (car rule))
                             (folder  (cdr rule)))
                         (cons account (format "\"%s\" %s"
                                               folder tags))))
                     (with-temp-buffer
                       (insert-file-contents "~/.emacs.d/secret/notmuch-fcc")
                       ;; Format:
                       ;;   (("me@example.com"   . "me@example.com/Sent")
                       ;;    ("alsome@gmail.com" . "alsome@gmail.com/Sent Mail"))
                       (goto-char (point-min))
                       (read (current-buffer))))))
            (setq message-signature
                  (lambda ()
                    (when (eq this-command #'message-insert-signature)
                      (let* ((signature-override
                              (concat (file-name-as-directory "~/.signature.d")
                                      (message-sendmail-envelope-from)))
                             (signature-file
                              (if (file-readable-p signature-override)
                                  signature-override
                                "~/.signature")))
                        (when (file-readable-p signature-file)
                          (with-temp-buffer
                            (insert-file-contents signature-file)
                            (buffer-string)))))))

            (setq message-sendmail-envelope-from 'header
                  send-mail-function 'sendmail-send-it)
            (setq notmuch-always-prompt-for-sender t)

            (setq notmuch-wash-signature-lines-max 3)

            (when (file-executable-p "~/.bin/notmuch-sync")
              (defun my-notmuch-poll-and-refresh-this-buffer ()
                (interactive)
                (call-process
                 "notmuch-sync" nil 0 nil
                 (buffer-name (current-buffer)))))

            (dolist (map (list notmuch-hello-mode-map
                               notmuch-show-mode-map))
              (unbind-key "<C-tab>" map))
            (dolist (map (list notmuch-hello-mode-map
                               notmuch-show-mode-map
                               notmuch-search-mode-map))
              (when (file-executable-p "~/.bin/notmuch-sync")
                (bind-key "G" #'my-notmuch-poll-and-refresh-this-buffer
                          map)))

            (bind-key "A"
                      (lambda ()
                        (interactive)
                        (when (y-or-n-p "Archive all?")
                          (notmuch-search-tag-all '("-unread" "-inbox"))))
                      notmuch-search-mode-map)
            (bind-key "D"
                      (lambda ()
                        (interactive)
                        (when (y-or-n-p "Delete all?")
                          (notmuch-search-tag-all '("-unread" "-inbox" "+deleted"))))
                      notmuch-search-mode-map)


            (defun notmuch-clear-search-history ()
              (interactive)
              (when (y-or-n-p "Clear the notmuch search history? ")
                (setq notmuch-search-history nil)
                (notmuch-refresh-this-buffer)))
            (bind-key "d" #'notmuch-clear-search-history
                      notmuch-hello-mode-map)
            (bind-key "C-c C-o"
                      (lambda (arg)
                        (interactive "P")
                        (if arg
                            (progn
                              (require 'shr)
                              (shr-next-link))
                          (require 'ffap)
                          (ffap-next-url)))
                      notmuch-show-mode-map)

            (bind-key "O"
                      (lambda () (interactive)
                        (setq notmuch-search-oldest-first
                              (not notmuch-search-oldest-first))
                        (message "Notmuch search oldest first: %s"
                                 notmuch-search-oldest-first))
                      notmuch-hello-mode-map)

            (defun notmuch-fcc-replace ()
              (interactive)
              (message-remove-header "Fcc")
              (notmuch-fcc-header-setup))

            (setq mm-tmp-directory (file-name-as-directory
                                    (concat
                                     "/tmp/mml-" (user-login-name))))
            (make-directory mm-tmp-directory t)

            (setq mml-secure-openpgp-sign-with-sender t)))

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
                      (call-interactively 'cua-toggle-global-mark)))
          (advice-add 'cua--deactivate-global-mark :after
                      (lambda (ret-value)
                        (cua-mode 0)
                        ret-value))))

(use-package slime
  :straight t
  :defer t
  :config (when (file-readable-p "~/quicklisp/slime-helper.el")
            (load "~/quicklisp/slime-helper.el")))
(use-package slime-autoloads
  :config (setq inferior-lisp-program "sbcl"))

(use-package rust-mode :straight t :defer t)

(use-package go-mode :straight t :defer t)

(use-package lua-mode :straight t :defer t)

(load "my-theme")


(when (file-exists-p "~/.emacs.d/local.el")
  (load "~/.emacs.d/local.el"))

(use-package nlinum
  :straight t
  :defer t
  :if (not (fboundp #'display-line-numbers-mode)))

(use-package midnight
  :defer 13
  :config (midnight-mode 1))

(use-package hyperlist-mode
  :straight t
  :mode ("\\.hl\\'" . hyperlist-mode))

(use-package aggressive-indent :straight t :defer t)
(use-package color-identifiers-mode :straight t :defer t)
(use-package dockerfile-mode :straight t :defer t)
(use-package nix-mode :straight t :defer t)
(use-package dpaste :straight t :defer t)
(use-package impatient-mode :straight t :defer t)
(use-package rainbow-mode :straight t :defer t)
(use-package restclient :straight t :defer t)
(use-package web-beautify :straight t :defer t)
(use-package wgrep :straight t :defer t)
(use-package ripgrep :straight t :defer t)
(use-package orgalist :straight t :defer t)
(use-package chronos :straight t :defer t)

;;; Needs to be the last one because otherwise during the installation
;;; (via :ensure) it prompts whether to save ~/.abbrev_defs making it
;;; no longer fully automated. Sadly I've got no idea why it happens.
(use-package ess
  :straight t
  :defer t
  :config (setq ess-keep-dump-files nil
                ess-delete-dump-files t
                ess-history-file nil))
