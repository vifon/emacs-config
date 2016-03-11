(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold))
          'append)
(setq gc-cons-threshold most-positive-fixnum)

(add-to-list 'load-path "~/.emacs.d/my-fixes")
(add-to-list 'load-path "~/.emacs.d/vifon")
(add-to-list 'load-path "~/.emacs.d/modules")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)
(require 'my-compat)

;; (setq package-archives
;;       '(("gnu"       . "http://elpa.gnu.org/packages/")
;;         ("marmalade" . "http://marmalade-repo.org/packages/")
;;         ("elpa"      . "http://tromey.com/elpa/")
;;         ("melpa"     . "http://melpa.org/packages/")))

(require 'cask "~/.cask/cask.el")
(add-to-list 'auto-mode-alist '("/Cask\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("/emacs\\'" . emacs-lisp-mode))
(cask-initialize)
(setq package-enable-at-startup nil)
(package-initialize)
(require 'use-package)

(use-package key-chord
  :config (progn
            (setq key-chord-two-keys-delay 0.050
                  key-chord-one-key-delay  0.070)
            (key-chord-mode 1)))

(use-package hydra)


(require 'my-hooks)
(require 'my-skeletons)
(require 'my-mode-line)
(require 'my-fun)
(require 'my-keys)
(require 'my-god-mode)
(require 'my-helm)
(require 'my-ibuffer)
(require 'my-org)
(require 'my-eshell)
(require 'my-fixes)
(require 'my-registers)
(require 'my-global-settings)
(require 'my-scratch)

(require 'pastes-from-web)

(use-package ido
  :defer t
  :init (setq ido-vertical-define-keys nil)
  :config (progn
            (flx-ido-mode 1)
            (ido-vertical-mode 1)
            (add-to-list 'ido-work-directory-list-ignore-regexps
                         tramp-file-name-regexp)
            (setq ido-enable-flex-matching t
                  ido-enable-tramp-completion nil
                  ido-default-file-method 'selected-window
                  ido-default-buffer-method 'selected-window
                  ido-auto-merge-work-directories-length -1
                  ido-use-virtual-buffers t)
            (add-hook 'ido-setup-hook
                      (defun my-ido-keys ()
                        (define-key ido-file-dir-completion-map
                          (kbd "M-a") 'ido-merge-work-directories)
                        (define-key ido-completion-map
                          (kbd "C-n") 'ido-next-match)
                        (define-key ido-completion-map
                          (kbd "C-p") 'ido-prev-match)))))

(use-package globalff
  :commands globalff)

(use-package paredit
  :diminish "[()]"
  :commands paredit-mode
  :init (setq paredit-space-for-delimiter-predicates
              '((lambda (endp delimiter) nil)))
  :config (progn
            (define-key paredit-mode-map (kbd "M-s") nil)
            (define-key paredit-mode-map (kbd "M-s M-s") 'paredit-splice-sexp)

            (defun paredit-kill-maybe (arg)
              (interactive "P")
              (if (consp arg)
                  (paredit-kill)
                  (kill-line arg))))
  :bind (([remap kill-line] . paredit-kill-maybe)))

(use-package autopair
  :commands autopair-mode)

(use-package corral
  :bind (("M-(" . corral-parentheses-forward)
         ("M-)" . corral-parentheses-backward)))

(use-package hideshow
  :defer t
  :config (progn
            (key-chord-define hs-minor-mode-map
                              "`;" 'hs-hide-level)
            (define-key hs-minor-mode-map (kbd "<M-left>") 'hs-hide-block)
            (define-key hs-minor-mode-map (kbd "<M-right>") 'hs-show-block)
            (define-key hs-minor-mode-map (kbd "<M-up>") 'hs-hide-level)
            (define-key hs-minor-mode-map (kbd "<M-down>") 'hs-show-all)))

(use-package yafolding
  :defer t
  :init (add-hook 'prog-mode-hook #'yafolding-mode)
  :config (defadvice yafolding-go-parent-element
              (around yafolding-python-fix activate)
            (if (eq major-mode 'python-mode)
                (python-nav-backward-up-list)
                ad-do-it)))

(use-package hideshow-org
  :commands hs-org/minor-mode)

(use-package dired-x
  :init (setq dired-x-hands-off-my-keys t))

(use-package dired
  :defer t
  :config (progn
            (define-key dired-mode-map (kbd "z") #'dired-subtree-toggle)))

(use-package yasnippet
  :defer 7
  :diminish yas-minor-mode
  :commands yas-global-mode
  :init (progn (setq yas-trigger-key "TAB"
                     yas-snippet-dirs '("~/.emacs.d/snippets")
                     yas-prompt-functions '(yas/dropdown-prompt
                                            yas/ido-prompt
                                            yas/x-prompt
                                            yas/completing-prompt
                                            yas/no-prompt)
                     auto-mode-alist (cons '("emacs\.d/snippets/" . snippet-mode)
                                           auto-mode-alist)))
  :config (progn
            (use-package auto-yasnippet
              :bind (("C-c Y" . aya-create)
                     ("C-c y" . aya-expand-with-indent))
              :config (defun aya-expand-with-indent (arg)
                        (interactive "P")
                        (aya-expand)
                        (unless arg
                          (indent-for-tab-command))))
            (yas-global-mode 1)))

(use-package tiny
  :bind ("C-c M-y" . tiny-expand))

(use-package undo-tree
  :diminish undo-tree-mode
  :config (global-undo-tree-mode 1))

(use-package auctex
  :defer t
  :init (setq preview-scale-function 2.0)
  :config (defun my-auctex-build-pdf ()
            (interactive)
            (TeX-command "LaTeX" 'TeX-master-file)))

(use-package hippie-expand-ido
  :bind ("C-c /" . my-ido-hippie-expand))

(use-package legalese
  :config (defun legalese-box (ask)
            (interactive "P")
            (let ((comment-style 'box))
              (legalese ask))))

(use-package minimap
  :bind ("C-c n" . minimap-toggle)
  :config (defun minimap-toggle ()
            (interactive)
            (if (and minimap-bufname
                     (get-buffer minimap-bufname)
                     (get-buffer-window (get-buffer minimap-bufname)))
                (minimap-kill)
                (minimap-create))))

(use-package transpose-frame
  :bind (("C-x 4 t" . transpose-frame)
         ("C-x 4 i" . flop-frame)
         ("C-x 4 I" . flip-frame)))

(use-package ace-window
  :bind ("C-x O" . ace-window))

(use-package vim-line-open
  :bind ("C-o" . open-next-line-dwim))

(use-package evil-nerd-commenter
  :init (defun evil-nerd-commenter-dwim (arg)
          (interactive "P")
          (if arg
              (evilnc-comment-or-uncomment-lines arg)
              (call-interactively 'comment-dwim)))
  :bind (([remap comment-dwim] . evil-nerd-commenter-dwim)))

(use-package neotree
  :bind (("C-c b" . neotree-toggle)
         ("C-c B" . neotree-find)))

(use-package magit
  :bind (("C-c g" . magit-status))
  :init (progn
          (key-chord-define-global "`m" 'magit-status)
          (setq magit-last-seen-setup-instructions "1.4.0"))
  :config (mapcar
           (lambda (keymap)
             (define-key keymap (kbd "M-<tab>") #'magit-section-cycle)
             (define-key keymap (kbd "C-<tab>") nil))
           (list magit-status-mode-map
                 magit-log-mode-map
                 magit-reflog-mode-map
                 magit-refs-mode-map
                 magit-diff-mode-map)))

(use-package git-commit
  :defer t
  :config (progn
            (define-key git-commit-mode-map (kbd "C-c C-l") 'magit-log)
            (add-hook 'git-commit-mode-hook 'turn-on-orgstruct++)))

(use-package git-messenger
  :bind ("C-x v p" . git-messenger:popup-message))

(use-package git-timemachine
  :bind ("C-x v t" . git-timemachine))

(use-package stgit)

(use-package quilt)

(use-package p4
  :if (file-exists-p "~/.p4config"))

(use-package ed
  :commands ed)

(use-package goto-last-change
  :demand t
  :bind ("C-x C-\\" . goto-last-change))

(use-package highlight-symbol
  :demand t
  :bind (("<f10>"   . highlight-symbol-at-point)
         ("<C-f10>" . highlight-symbol-reset)
         ("<f11>"   . highlight-symbol-prev)
         ("M-p"     . highlight-symbol-prev)
         ("<f12>"   . highlight-symbol-next)
         ("M-n"     . highlight-symbol-next))
  :init (key-chord-define-global "nl" 'highlight-symbol-at-point)
  :config (defun highlight-symbol-reset ()
            (interactive)
            (highlight-symbol-remove-all)
            (setq highlight-symbol-color-index 0)))

(use-package bm
  :commands (bm-load-and-restore)
  :bind ("<left-fringe> <mouse-1>" . bm-toggle-mouse))

(use-package breadcrumb
  :bind (("C-x C-SPC" . breadcrumb-or-pop))
  :init (progn
          (defhydra hydra-breadcrumb
            (:exit t :hint nil)
            "
Breadcrumb bookmarks:
  _p_: prev   _P_: local prev
  _n_: next   _N_: local next
  _s_: set  _c_: clear  _l_: list  _q_: quit
"
            ("n" bc-next nil :exit nil)
            ("p" bc-previous nil :exit nil)
            ("N" bc-local-next nil :exit nil)
            ("P" bc-local-previous nil :exit nil)
            ("l" bc-list nil)
            ("s" bc-set nil)
            ("SPC" bc-set)
            ("c" bc-clear nil)
            ("q" nil nil)
            ("S" (lambda ()
                   (interactive)
                   (bc-bookmarks-save))))
          (defun breadcrumb-or-pop (arg)
            (interactive "P")
            (if arg
                (call-interactively 'pop-global-mark)
                (call-interactively 'hydra-breadcrumb/body)))))

(use-package indent-guide
  :defer t
  :init (add-hook 'LaTeX-mode-hook (lambda ()
                                     (indent-guide-mode 1)))
  :config (setq indent-guide-delay nil))

(use-package writeroom-mode
  :defer t
  :config (setq writeroom-global-effects
                (delete 'writeroom-toggle-fullscreen
                        writeroom-global-effects)))

(use-package visual-regexp-steroids
  :bind (([remap query-replace-regexp] . vr/query-replace)))

(use-package flx-isearch
  :bind (([remap isearch-forward-regexp]  . flx-isearch-forward)
         ([remap isearch-backward-regexp] . flx-isearch-backward)))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode 1))

(use-package beacon
  :config (beacon-mode 1))

(use-package column-marker
  :bind ("C-c u" . column-marker-1))

(use-package multiple-cursors
  :bind (("C-<"         . mc/mark-previous-like-this)
         ("C->"         . mc/mark-next-like-this)
         ("C-+"         . mc/mark-next-like-this)
         ("C-c H h"     . mc/edit-lines)
         ("C-c H C-e"   . mc/edit-ends-of-lines)
         ("C-c H C-a"   . mc/edit-beginnings-of-lines)
         ("C-c H r"     . mc/reverse-regions)
         ("C-c H C-SPC" . set-rectangular-region-anchor)
         ("C-*"         . mc/mark-all-like-this-dwim)
         ("C-c H e"     . mc/mark-more-like-this-extended)
         ("M-<mouse-1>" . mc/add-cursor-on-click))
  :init (progn
          (global-unset-key (kbd "M-<down-mouse-1>"))
          (key-chord-define-global "[e" 'mc/mark-previous-like-this)
          (key-chord-define-global "]e" 'mc/mark-next-like-this)
          (use-package phi-search
            :bind (("C-s" . phi-search-dwim)
                   ("C-r" . phi-search-backward-dwim))
            :config (progn
                      (defun phi-search-dwim (arg)
                        (interactive "P")
                        (if arg
                            (phi-search)
                            (call-interactively 'isearch-forward)))
                      (defun phi-search-backward-dwim (arg)
                        (interactive "P")
                        (if arg
                            (phi-search-backward)
                            (call-interactively 'isearch-backward))))))
  :config (progn
            (define-key mc/keymap (kbd "C-s") 'phi-search)
            (define-key mc/keymap (kbd "C-r") 'phi-search-backward)))

(use-package multifiles
  :bind ("C-!" . mf/mirror-region-in-multifile))

(use-package iedit
  :bind ("C-;" . iedit-mode))

(use-package vcursor
  :defer t
  :init (setq vcursor-key-bindings t)
  :config (global-set-key (kbd "C-M-<tab>") 'vcursor-swap-point))

(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :init (key-chord-define-global "'e" 'er/expand-region))

(use-package swiper
  :bind ("C-c f" . swiper-dwim)
  :commands (swiper-from-isearch)
  :init (define-key isearch-mode-map (kbd "C-i") #'swiper-from-isearch)
  :config (defun swiper-dwim (arg)
            (interactive "P")
            (if arg
                (ivy-resume)
              (swiper))))

(use-package counsel
  :bind (("C-c G" . counsel-git)
         ("M-X" . counsel-M-x)))

(use-package imenu
  :bind ("C-c k" . helm-semantic-or-imenu)
  :init (add-hook 'emacs-lisp-mode-hook
                  (lambda ()
                    (add-to-list 'imenu-generic-expression
                                 '("Used Packages"
                                   "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))))

(use-package semantic
  :defer t
  :config (progn
            (setq semantic-default-submodes
                  '(global-semantic-idle-scheduler-mode
                    global-semanticdb-minor-mode
                    global-semantic-decoration-mode
                    global-semantic-stickyfunc-mode))
            (use-package semantic/decorate/mode
              :config (setq-default semantic-decoration-styles
                                    '(("semantic-decoration-on-includes" . t))))))

(use-package srefactor
  :defer t
  :init (progn
          (defun srefactor-c-common-hook ()
            (local-set-key (kbd "C-c C-c") #'srefactor-refactor-at-point))
          (add-hook 'c-mode-common-hook #'srefactor-c-common-hook)))

(use-package company
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
            (add-to-list 'company-backends 'company-tern)
            (global-company-mode 1))
  :init (progn
          (use-package company-clang
            :bind ("C-c V" . company-clang))
          (add-hook 'c++-mode-hook
                    (lambda ()
                      (make-local-variable 'company-clang-arguments)
                      (setq company-clang-arguments '("-std=c++11")))))
  :bind ("C-c v" . company-complete))

(use-package ggtags
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
  :defer t
  :init (progn
          (add-hook 'c-mode-hook (lambda ()
                                    (setq flycheck-clang-language-standard "c99")))
          (add-hook 'c++-mode-hook (lambda ()
                                     (setq flycheck-clang-language-standard "c++11")))))

(use-package cpputils-cmake
  :commands (cppcm-reload-all cppcm-get-exe-path-current-buffer)
  :init (progn
          ;; (add-hook 'c-mode-common-hook
          ;;           (defun my-cpputils-cmake-hook ()
          ;;             (if (derived-mode-p 'c-mode 'c++-mode)
          ;;                 (cppcm-reload-all))))
          (defun gdb-dwim ()
            (interactive)
            (gud-gdb (concat gud-gdb-command-name
                             " --fullname "
                             (cppcm-get-exe-path-current-buffer)))))
  :config (setq cppcm-write-flymake-makefile nil))

(use-package racer
  :if (file-directory-p "~/src/racer/editors")
  :load-path "~/src/racer/editors"
  :config (progn
            (setq racer-rust-src-path "/home/vifon/src/rust/src")
            (setq racer-cmd "racer")
            (eval-after-load "rust-mode" '(require 'racer))))

(use-package bbyac
  :diminish bbyac-mode
  :config (progn
            (bbyac-global-mode 1)
            (define-key bbyac-mode-map (kbd "M-?") 'bbyac-expand-partial-lines)
            (define-key bbyac-mode-map (kbd "C-M-/") 'bbyac-expand-symbols)))

(use-package sx
  :defer t
  :init (use-package sx-search
          :defer t
          :commands sx-search))

(use-package avy-jump
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
  :bind ("C-x o" . win-switch-dispatch)
  :config (setq win-switch-window-threshold 0))

(use-package popwin
  :defer t
  :config (progn
            (setq popwin:popup-window-height 25)))

(use-package tabbar
  :commands tabbar-mode)

(use-package bbcode-mode
  :commands bbcode-mode)

(use-package c-c++-header
  :mode ("\\.h\\'" . c-c++-header)
  :init (defalias 'h++-mode 'c++-mode))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets
                uniquify-strip-common-suffix t))

(use-package deft
  :defer t
  :bind (("<f5>" . deft))
  :init (setq deft-auto-save-interval 0)
  :config (progn
            (setq deft-extensions (sort deft-extensions
                                        (lambda (a b)
                                          (equal a "md"))))
            (setq deft-markdown-mode-title-level 1)))

(use-package markdown-mode
  :defer t
  :config (add-hook 'markdown-mode-hook
                    (defun my-markdown-mode-hook ()
                      (add-to-list (make-local-variable 'electric-pair-pairs)
                                   '(?` . ?`)))))

(use-package yaml-mode
  :defer t
  :config (progn
            (add-hook 'yaml-mode-hook (lambda () (ansible 1)))
            (add-hook 'yaml-mode-hook
                      (lambda ()
                        (add-to-list (make-local-variable 'company-backends)
                                     'company-ansible)))))

(use-package pod-mode
  :mode ("\\.pod$" . pod-mode))

(use-package crontab-mode
  :mode ("^/tmp/crontab\\..*" . crontab-mode))

(use-package haskell-mode
  :defer t
  :config (progn
            (setq haskell-program-name "cabal repl")
            (add-hook 'haskell-mode-hook #'interactive-haskell-mode)))

(use-package cperl-mode
  :commands cperl-mode
  :init (defalias 'perl-mode 'cperl-mode)
  :config (load "~/.emacs.d/my-fixes/cperl-lineup.el"))

(use-package python
  :defer t
  :config (progn
            (defun my-python-hook ()
              (company-mode 0)
              (setq tab-width 4
                    python-indent 4
                    py-indent-offset 4))
            (add-hook 'python-mode-hook 'my-python-hook)
            (if (file-exists-p "~/.emacs.d/.python-environments")
                (add-hook 'python-mode-hook 'jedi:setup))
            (setq jedi:complete-on-dot t)))

(use-package jedi
  :defer t
  :config (progn
            (define-key jedi-mode-map (kbd "C-<tab>") nil)
            (define-key jedi-mode-map (kbd "C-c v") 'jedi:complete)
            (let ((tag-functions (if (version<= "25.0" emacs-version)
                                     '(xref-find-definitions xref-pop-marker-stack)
                                   '(find-tag pop-tag-mark))))
              (define-key jedi-mode-map (vector 'remap (first tag-functions))
                'jedi:goto-definition)
              (define-key jedi-mode-map (vector 'remap (second tag-functions))
                'jedi:goto-definition-pop-marker))))

(use-package js-mode
  :defer t
  :config (progn
            (setq js-indent-level 2)
            (add-hook 'js-mode-hook (lambda () (tern-mode 1)))))

(use-package scala-mode2
  :config (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package ensime
  :defer t
  :config (progn
            (define-key ensime-mode-map (kbd "M-n") nil)
            (define-key ensime-mode-map (kbd "M-p") nil)
            (define-key ensime-mode-map [remap next-error] #'ensime-forward-note)
            (define-key ensime-mode-map [remap previous-error] #'ensime-backward-note)))

(use-package projectile
  :defer 3
  :commands (projectile-global-mode
             projectile-switch-project
             my-projectile-show-path)
  :init (progn
          (setq projectile-switch-project-action (lambda ()
                                                   (dired ".")))
          (key-chord-define-global "`p" 'projectile-switch-project))
  :bind ("C-c p p" . projectile-switch-project)
  :config (progn
            (setq projectile-mode-line
                  '(:eval
                    (format " Pro[%s]"
                            (projectile-project-name))))
            (define-key projectile-command-map (kbd "C-b") 'helm-projectile-buffers)
            (define-key projectile-command-map [?h] 'helm-browse-project)
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
            (define-key projectile-command-map (kbd "C-f") #'my-projectile-show-path)))

(use-package helm-ag
  :bind ("C-c p s S" . projectile-helm-ag)
  :config (progn
            (require 'projectile)
            (defun projectile-helm-ag ()
              (interactive)
              (helm-ag (projectile-project-root)))))

(use-package sane-term
  :init (setq sane-term-shell-command "/bin/zsh")
  :config (add-hook 'term-mode-hook
                    (lambda ()
                      (yas-minor-mode -1)))
  :bind (("C-x t" . sane-term)
         ("C-x T" . sane-term-create)))

(use-package diff-hl
  :defer 2
  :init (key-chord-define-global "=f" 'diff-hl-mode)
  :bind ("C-x v \\" . diff-hl-amend-mode)
  :config (progn
            (global-diff-hl-mode 1)))

(use-package recentf-merge
  :init (setq recentf-max-menu-items 100))

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

(use-package slime-autoloads
  :config (setq inferior-lisp-program "sbcl"))


(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)


(add-hook 'after-init-hook (lambda () (load-theme 'zenburn)))
(setq frame-background-mode 'dark)
(ignore-errors
  (let ((font-name "DejaVu Sans Mono")
        (font-size "11"))
    (let ((font (concat font-name "-" font-size)))
      (add-to-list 'default-frame-alist `(font . ,font))
      (set-frame-font font nil t))))

(use-package my-mu4e
  :if (and (file-directory-p "~/.emacs.d/secret")
           (file-directory-p "~/pkgs/mu"))
  :defer 17
  :bind ("<f5>" . mu4e)
  :config (require 'my-mu4e)
  :load-path ("~/.emacs.d/secret"
              "~/local/share/emacs/site-lisp/mu4e"))
(use-package my-secret
  :if (file-directory-p "~/.emacs.d/secret")
  :load-path ("~/.emacs.d/secret"))
