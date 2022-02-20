;;; -*- lexical-binding: t; -*-

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
            (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
            (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package magit-todos :straight t :defer t)

(use-package forge
  :straight t
  :after magit)

(use-package git-commit
  :straight t
  :mode ("/COMMIT_EDITMSG\\'" . git-commit-mode)
  :bind (:map git-commit-mode-map
         ("C-c C-l" . magit-log))
  :config (add-hook 'git-commit-mode-hook #'flyspell-mode))

(use-package git-timemachine
  :straight t
  :bind ("C-x v t" . git-timemachine))

(use-package git-link
  :straight t
  :bind ("C-x v w" . git-link)
  :config (setq git-link-default-branch "master"))

(use-package diff-hl
  :straight t
  :defer 2
  :bind ("C-x v \\" . diff-hl-amend-mode)
  :commands (diff-hl-magit-pre-refresh
             diff-hl-magit-post-refresh)
  :config (global-diff-hl-mode 1))
