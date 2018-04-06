(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold))
          'append)
(setq gc-cons-threshold most-positive-fixnum)

(defun bootstrap-use-package ()
  "Load `use-package' possibly installing it beforehand"
  (if (locate-library "package")
      (progn
        (require 'package)
        (setq package-user-dir (concat
                                (file-name-as-directory package-user-dir)
                                emacs-version))
        (setq package-archives
              '(("gnu"          . "https://elpa.gnu.org/packages/")
                ("marmalade"    . "https://marmalade-repo.org/packages/")
                ("elpa"         . "http://tromey.com/elpa/")
                ("melpa"        . "https://melpa.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("org"          . "https://orgmode.org/elpa/"))
              package-archive-priorities
              '(("org"          . 20)
                ("melpa-stable" . 15)
                ("gnu"          . 10)
                ("melpa"        . 5)
                ("marmalade"    . 0)
                ("elpa"         . 0)))
        (setq package-enable-at-startup nil)
        (package-initialize)
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        (require 'use-package))
    (message "WARNING: Ancient emacs! No advice-add, package.el")
    (defmacro advice-add (&rest body))
    (defmacro use-package (&rest body)))
  (use-package diminish :ensure t :defer t)
  (use-package bind-key :ensure t :defer t))
(bootstrap-use-package)


(add-to-list 'load-path "~/.emacs.d/vifon")
(add-to-list 'load-path "~/.emacs.d/modules")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(use-package hydra :ensure t :defer t)

(require 'my-hooks)
(require 'my-skeletons)
(require 'my-mode-line)
(require 'my-fun)
(require 'my-keys)
(require 'my-ivy)
(require 'my-ibuffer)
(require 'my-org)
(require 'my-eshell)
(require 'my-registers)
(require 'my-settings)
(require 'my-scratch)

(require 'pastes-from-web)


(use-package s :ensure t :defer t)
(use-package paredit
  :ensure t
  :diminish "[()]"
  :commands (paredit-mode paredit-kill)
  :init (progn
          (setq paredit-space-for-delimiter-predicates
                '((lambda (endp delimiter) nil)))
          (defun paredit-kill-maybe (arg)
            (interactive "P")
            (if (consp arg)
                (paredit-kill)
              (kill-line arg)))
          (add-hook 'emacs-lisp-mode-hook #'paredit-mode))
  :config (progn
            (define-key paredit-mode-map (kbd "M-s") nil)
            (define-key paredit-mode-map (kbd "M-s M-s") 'paredit-splice-sexp))
  :bind (([remap kill-line] . paredit-kill-maybe)))

(use-package autopair
  :ensure t
  :commands autopair-mode)

(use-package yafolding
  :ensure t
  :defer t
  :init (add-hook 'prog-mode-hook #'yafolding-mode)
  :config (defadvice yafolding-go-parent-element
              (around yafolding-python-fix activate)
            (if (eq major-mode 'python-mode)
                (python-nav-backward-up-list)
              ad-do-it)))

(use-package hideshow-org
  :ensure t
  :commands hs-org/minor-mode)

(use-package dired-x
  :init (setq dired-x-hands-off-my-keys t))

(use-package dired-subtree
  :ensure t
  :commands dired-subtree-toggle)

(use-package dired-filter
  :ensure t
  :config (setq-default dired-filter-stack '()))

(use-package dired-collapse
  :ensure t)

(use-package dired-async
  :ensure async
  :commands dired-async-mode
  :config (setq dired-async-message-function
                (lambda (text face &rest args)
                  (call-process "notify-send" nil nil nil
                                "Emacs: dired-async"
                                (apply #'format text args))
                  (apply #'dired-async-mode-line-message text face args))))

(use-package dired
  :defer t
  :bind (:map dired-mode-map
         ("z" . dired-subtree-toggle)
         ("C-c D" . dired-submodes-hydra/body))
  :config (setq dired-dwim-target t
                dired-listing-switches "-alhv --group-directories-first"
                dired-ls-F-marks-symlinks t
                dired-isearch-filenames 'dwim))

(use-package yasnippet
  :ensure t
  :defer 7
  :diminish yas-minor-mode
  :commands yas-global-mode
  :mode ("emacs\.d/snippets/" . snippet-mode)
  :init (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config (yas-global-mode 1))

(use-package auto-yasnippet
  :ensure t
  :bind (("C-c Y" . aya-create)
         ("C-c y" . aya-expand-with-indent))
  :config (defun aya-expand-with-indent (arg)
            (interactive "P")
            (aya-expand)
            (unless arg
              (indent-for-tab-command))))

(use-package tiny
  :ensure t
  :bind ("C-c M-y" . tiny-expand))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config (progn
            (global-undo-tree-mode 1)
            (setq undo-tree-enable-undo-in-region nil)))

(use-package auctex
  :ensure t
  :defer t
  :init (setq preview-scale-function 2.0)
  :config (defun my-auctex-build-pdf ()
            (interactive)
            (TeX-command "LaTeX" 'TeX-master-file)))

(use-package web-mode
  :ensure t
  :mode (("\\.html\\'" . web-mode)
         ("\\.html\\.j2\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode))
  :config (progn
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2)
            (setq web-mode-engines-alist
                  '(("jinja" . "\\.j2\\'")
                    ("jinja" . "\\.html\\'")))
            ;; Fix Jinja2 autopairing; was producing: "{{  }}}}".
            (setq web-mode-enable-auto-pairing nil)
            (define-abbrev web-mode-abbrev-table "divc"
              ""
              (define-skeleton html-div-skeleton
                "A <div></div> tag pair with a class."
                "Class: "
                "<div class=\""str"\">" _ "</div>"))))

(use-package legalese
  :ensure t
  :commands legalese
  :init (defun legalese-box (ask)
          (interactive "P")
          (let ((comment-style 'box))
            (legalese ask))))

(use-package minimap
  :ensure t
  :bind ("C-c n" . minimap-mode))

(use-package transpose-frame
  :ensure t
  :bind (("C-x 4 t" . transpose-frame)
         ("C-x 4 i" . flop-frame)
         ("C-x 4 I" . flip-frame)))

(use-package vim-line-open
  :bind ("C-o" . open-next-line-dwim))

(use-package evil-nerd-commenter
  :ensure t
  :init (defun evil-nerd-commenter-dwim (arg)
          (interactive "P")
          (if arg
              (evilnc-comment-or-uncomment-lines arg)
            (call-interactively 'comment-dwim)))
  :bind (([remap comment-dwim] . evil-nerd-commenter-dwim)))

(use-package neotree
  :ensure t
  :bind (("C-c b" . neotree-toggle)
         ("C-c B" . neotree-find)))

(use-package magit
  :ensure t
  :bind (("C-c g" . magit-status)
         ("C-c G" . magit-file-popup))
  :init (progn
          (setq magit-last-seen-setup-instructions "1.4.0")
          (setq magit-diff-refine-hunk t)
          (setq magit-completing-read-function #'ivy-completing-read))
  :config (progn
           (mapcar
            (lambda (keymap)
              (define-key keymap (kbd "M-<tab>") #'magit-section-cycle)
              (define-key keymap (kbd "C-<tab>") nil))
            (list magit-status-mode-map
                  magit-log-mode-map
                  magit-reflog-mode-map
                  magit-refs-mode-map
                  magit-diff-mode-map))
           (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package git-commit
  :ensure t
  :config (progn
            (define-key git-commit-mode-map (kbd "C-c C-l") 'magit-log)
            (add-hook 'git-commit-mode-hook 'turn-on-orgstruct++)))

(use-package git-messenger
  :ensure t
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package git-timemachine
  :ensure t
  :bind ("C-x v t" . git-timemachine))

(use-package goto-last-change
  :ensure t
  :demand t
  :bind ("C-x C-\\" . goto-last-change))

(use-package highlight-symbol
  :ensure t
  :demand t
  :bind (("<f10>"   . highlight-symbol-at-point)
         ("<C-f10>" . highlight-symbol-reset)
         ("<f11>"   . highlight-symbol-prev)
         ("M-p"     . highlight-symbol-prev)
         ("<f12>"   . highlight-symbol-next)
         ("M-n"     . highlight-symbol-next))
  :config (defun highlight-symbol-reset ()
            (interactive)
            (highlight-symbol-remove-all)
            (setq highlight-symbol-color-index 0)))

(use-package bm
  :ensure t
  :commands (bm-load-and-restore)
  :bind ("<left-fringe> <mouse-1>" . bm-toggle-mouse))

(use-package indent-guide
  :ensure t
  :defer t
  :init (add-hook 'LaTeX-mode-hook (lambda ()
                                     (indent-guide-mode 1)))
  :config (setq indent-guide-delay nil))

(use-package writeroom-mode
  :ensure t
  :defer t
  :config (setq writeroom-global-effects
                (delete 'writeroom-toggle-fullscreen
                        writeroom-global-effects)))

(use-package visual-regexp :ensure t :defer t)
(use-package visual-regexp-steroids
  :ensure t
  :after visual-regexp
  :bind (([remap query-replace-regexp] . vr/query-replace)))

(use-package flx-isearch
  :ensure t
  :bind (([remap isearch-forward-regexp]  . flx-isearch-forward)
         ([remap isearch-backward-regexp] . flx-isearch-backward)))

(use-package volatile-highlights
  :ensure t
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode 1))

(use-package beacon
  :ensure t
  :config (beacon-mode 1))

(use-package phi-search
  :ensure t
  :commands (phi-search phi-search-backward))
(use-package multiple-cursors
  :ensure t
  :demand t                             ;C-v/M-v don't work otherwise
  :bind (("C-<"         . mc/mark-previous-like-this)
         ("C->"         . mc/mark-more-like-this-extended)
         ("C-*"         . mc/mark-all-like-this-dwim)
         ("C-M-;"       . mc/mark-all-like-this-dwim)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :config (progn
            (define-key mc/keymap (kbd "C-s") 'phi-search)
            (define-key mc/keymap (kbd "C-r") 'phi-search-backward)))

(use-package iedit
  :ensure t
  :bind ("C-;" . iedit-mode))

(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
         ("M-S-<SPC>" . er/expand-region)))

(use-package imenu
  :init (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (add-to-list 'imenu-generic-expression
                                 '("Used Packages"
                                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))))

(use-package semantic/decorate/mode
  :defer t
  :config (setq-default semantic-decoration-styles
                        '(("semantic-decoration-on-includes" . t))))

(use-package semantic
  :defer t
  :config (setq semantic-default-submodes
                '(global-semantic-idle-scheduler-mode
                  global-semanticdb-minor-mode
                  global-semantic-decoration-mode
                  global-semantic-stickyfunc-mode)))

(use-package company-clang
  :after company
  :bind ("C-c V" . company-clang))

(use-package company
  :ensure t
  :defer 5
  :config (progn
            (setq company-idle-delay 0.25)
            (setq company-backends '(company-bbdb
                                     company-nxml
                                     company-css
                                     company-eclim
                                     company-semantic
                                     ;; company-clang
                                     company-xcode
                                     company-cmake
                                     company-capf
                                     (company-gtags
                                      company-etags
                                      :with
                                      company-keywords
                                      company-dabbrev-code)
                                     company-clang ; moved down
                                     company-oddmuse
                                     company-files
                                     company-dabbrev))
            (add-hook 'eshell-mode-hook '(lambda ()
                                           (company-mode 0)))
            (add-hook 'org-mode-hook '(lambda ()
                                        (company-mode 0)))
            (global-company-mode 1))
  :init (add-hook 'c++-mode-hook
                  (lambda ()
                    (make-local-variable 'company-clang-arguments)
                    (setq company-clang-arguments '("-std=c++11"))))
  :bind (("C-c v" . company-complete)
         ("C-c /" . company-files)))

(use-package ggtags
  :ensure t
  :demand t        ;bad things happen in the globalized mode otherwise
  :init (define-globalized-minor-mode global-ggtags-mode ggtags-mode
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)))))

(use-package rtags
  :if (file-directory-p "~/local/share/emacs/site-lisp/rtags")
  :load-path "~/local/share/emacs/site-lisp/rtags"
  :commands (rtags-minor-mode global-rtags-minor-mode)
  :defer 5
  :config (progn
            (define-minor-mode rtags-minor-mode
              nil
              :lighter " RTags"
              :keymap (let ((m (make-sparse-keymap)))
                        (rtags-enable-standard-keybindings m (kbd "C-c C-r"))
                        (define-key m (kbd "M-.") #'rtags-find-symbol-at-point)
                        (define-key m (kbd "M-,") #'rtags-location-stack-back)
                        (define-key m (kbd "M-]") #'rtags-find-references-at-point)
                        m))
            (define-globalized-minor-mode global-rtags-minor-mode rtags-minor-mode
              (lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode)
                  (rtags-minor-mode 1))))
            (require 'company)
            (push 'company-rtags company-backends)
            (setq rtags-autostart-diagnostics t)
            (setq rtags-completions-enabled t)))

(use-package flycheck
  :ensure t
  :defer t
  :init (progn
          (add-hook 'c-mode-hook (lambda ()
                                   (setq flycheck-clang-language-standard "c99")))
          (add-hook 'c++-mode-hook (lambda ()
                                     (setq flycheck-clang-language-standard "c++11")))))

(use-package cmake-mode
  :ensure t
  :defer t)

(use-package avy
  :ensure t
  :bind ("C-c j" . avy-dwim)
  :init (progn
          (avy-setup-default)
          (defun avy-dwim (arg)
            (interactive "P")
            (cond ((not arg)
                   (call-interactively #'avy-goto-word-1))
                  ((equal arg '(4))
                   (call-interactively #'avy-goto-char))
                  ((equal arg '(16))
                   (avy-goto-line))
                  (t nil)))))

(use-package win-switch
  :ensure t
  :bind ("C-x o" . win-switch-dispatch)
  :config (setq win-switch-window-threshold 0))

(use-package winner
  :bind (("C-c z" . winner-undo)
         :map winner-mode-map
         ("<XF86Back>"    . winner-undo)
         ("<XF86Forward>" . winner-redo)))

(use-package c-c++-header
  :mode ("\\.h\\'" . c-c++-header)
  :init (defalias 'h++-mode 'c++-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
                uniquify-strip-common-suffix t))

(use-package deft
  :ensure t
  :bind (("<f5>" . deft))
  :init (setq deft-auto-save-interval 0)
  :config (setq deft-extensions (sort deft-extensions
                                      (lambda (a b)
                                        (equal a "md")))
                deft-default-extension "md"
                deft-markdown-mode-title-level 1))

(use-package markdown-mode
  :ensure t
  :defer t
  :config (add-hook 'markdown-mode-hook
                    (defun my-markdown-mode-hook ()
                      (add-to-list (make-local-variable 'electric-pair-pairs)
                                   '(?` . ?`)))))

(use-package ansible :ensure t :defer t)
(use-package ansible-doc :ensure t :defer t)
(use-package company-ansible :ensure t :defer t)

(use-package yaml-mode
  :ensure t
  :defer t
  :mode ("\\.sls\\'" . yaml-mode)       ;Saltstack files
  :config (progn
            (add-hook 'yaml-mode-hook (lambda () (ansible 1)))
            (add-hook 'yaml-mode-hook
                      (lambda ()
                        (add-to-list (make-local-variable 'company-backends)
                                     'company-ansible)))))

(use-package pod-mode
  :mode ("\\.pod$" . pod-mode))

(use-package dumb-jump
  :ensure t
  :defer t
  :bind ((:map dumb-jump-mode-map
          ("C-M-p" . nil)
          ("C-M-S-g" . dumb-jump-back)))
  :config (setq dumb-jump-selector 'ivy))

(use-package haskell-mode
  :ensure t
  :defer t
  :config (progn
            (setq haskell-program-name "cabal repl")
            (add-hook 'haskell-mode-hook #'interactive-haskell-mode)))

(use-package cperl-mode
  :commands cperl-mode
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
  :config (progn
            (define-key cperl-mode-map (kbd ".") #'perl-method-call-dwim)
            (require 'perltidy)
            (define-key cperl-mode-map (kbd "C-c C-i") #'perltidy-dwim-safe)))

(use-package python
  :defer t
  :config (progn
            (add-hook 'python-mode-hook
                      (defun my-python-hook ()
                        (setq tab-width 4
                              python-indent 4
                              py-indent-offset 4)))
            (setq flycheck-python-pylint-executable "pylint3")))

(use-package anaconda-mode
  :ensure t
  :commands (anaconda-mode
             anaconda-eldoc-mode)
  :init (progn
          (add-hook 'python-mode-hook #'anaconda-mode)
          (add-hook 'python-mode-hook #'anaconda-eldoc-mode)))

(use-package js-mode
  :defer t
  :init (setq-default js-indent-level 2))
(use-package typescript-mode
  :ensure t
  :defer t
  :init (setq-default typescript-indent-level 2))

(use-package projectile
  :ensure t
  :defer 3
  :commands (projectile-global-mode
             projectile-switch-project
             my-projectile-show-path)
  :init (progn
          (setq projectile-switch-project-action (lambda ()
                                                   (dired "."))))
  :bind ("C-c p p" . projectile-switch-project)
  :config (progn
            (setq projectile-mode-line
                  '(:eval
                    (format " Pro[%s]"
                            (projectile-project-name))))
            (projectile-global-mode 1)
            (defun my-projectile-show-path (arg)
              (interactive "P")
              (let ((project-path (if (projectile-project-p)
                                      (file-relative-name buffer-file-name
                                                          (projectile-project-root))
                                    buffer-file-name)))
                (when arg
                  (kill-new project-path))
                (message "%s" project-path)))
            (define-key projectile-command-map (kbd "C-f") #'my-projectile-show-path)
            (setq projectile-completion-system 'ivy)))

(use-package rg
  :ensure t
  :commands rg)

(use-package sane-term
  :ensure t
  :init (setq sane-term-shell-command "/bin/zsh")
  :config (add-hook 'term-mode-hook
                    (lambda ()
                      (yas-minor-mode -1)))
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))

(use-package zop-to-char
  :ensure t
  :bind (("M-Z" . zop-up-to-char)))

(use-package diff-hl
  :ensure t
  :defer 2
  :bind ("C-x v \\" . diff-hl-amend-mode)
  :config (progn
            (global-diff-hl-mode 1)))

(use-package recentf-merge
  :init (setq recentf-max-menu-items 10000))

(use-package image-mode
  :defer t
  :config (progn
            (define-key image-mode-map (kbd "k")
              (lambda (arg)
                (interactive "P")
                (if arg
                    (kill-buffer)
                  (kill-buffer-and-window))))
            (define-key image-mode-map (kbd "K")
              (lambda ()
                (interactive)
                (kill-buffer-and-window)))))

(use-package slime :ensure t :defer t)
(use-package slime-autoloads
  :config (setq inferior-lisp-program "sbcl"))


(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


(use-package zenburn-theme
  :ensure t
  :config (progn
            (load-theme 'zenburn 'no-confirm)
            (setq frame-background-mode 'dark)
            (ignore-errors
              (let ((font-name "DejaVu Sans Mono")
                    (font-size "11"))
                (let ((font (concat font-name "-" font-size)))
                  (add-to-list 'default-frame-alist `(font . ,font))
                  (set-frame-font font nil t))))))

(use-package my-secret
  :if (file-directory-p "~/.emacs.d/secret")
  :load-path ("~/.emacs.d/secret"))

(use-package aggressive-indent :ensure t :defer t)
(use-package color-identifiers-mode :ensure t :defer t)
(use-package dockerfile-mode :ensure t :defer t)
(use-package dpaste :ensure t :defer t)
(use-package esup :ensure t :defer t)   ;Startup profiler
(use-package focus-autosave-mode :ensure t :defer t)
(use-package hl-sexp :ensure t :defer t)
(use-package impatient-mode :ensure t :defer t)
(use-package nlinum :ensure t :defer t)
(use-package rainbow-mode :ensure t :defer t)
(use-package restclient :ensure t :defer t)
(use-package web-beautify :ensure t :defer t)

;;; Needs to be the last one because otherwise during the installation
;;; (via :ensure) it prompts whether to save ~/.abbrev_defs making it
;;; no longer fully automated. Sadly I've got no idea why it happens.
(use-package ess
  :ensure t
  :defer t
  :config (setq ess-keep-dump-files nil
                ess-delete-dump-files t
                ess-history-file nil))
