(require 'helm-config)
(use-package helm-mode
  :diminish helm-mode
  :config (progn
            (define-key helm-map (kbd "C-;") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
            (define-key helm-map (kbd "C-z") 'helm-select-action)))
(require 'helm-ls-git)
(require 'helm-help)

(use-package helm-swoop
  :defer t
  :commands helm-swoop-from-isearch
  :init (define-key isearch-mode-map (kbd "M-i") #'helm-swoop-from-isearch))

(setq helm-locate-command "locate %s -e -A %s")
(setq helm-buffer-max-length 40)

(setq helm-quick-update t
      helm-buffers-fuzzy-matching t
      helm-move-to-line-cycle-in-source nil)

(global-set-key (kbd "M-x")      #'helm-M-x)
(global-set-key (kbd "M-X")      #'execute-extended-command)
(global-set-key [remap occur]    #'helm-occur)
(global-set-key [remap yank-pop] #'helm-show-kill-ring)
(global-set-key (kbd "C-h C-f")  #'helm-apropos)
(global-set-key (kbd "C-x C-f")  #'helm-find-files)
(global-set-key (kbd "C-x b")    #'helm-buffers-list)
(global-set-key (kbd "M-s f")    #'helm-swoop)
(global-set-key [remap find-tag] #'helm-etags-select)

(defun my-helm-browse-project--basename (arg)
  (interactive "p")
  (let ((helm-ff-transformer-show-only-basename t))
    (helm-browse-project arg)))
(key-chord-define-global "`h" #'my-helm-browse-project--basename)
(global-set-key (kbd "C-c h") #'my-helm-browse-project--basename)

(setq helm-etags-execute-action-at-once-if-one nil)

(defun helm-custom ()
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename t))
    (helm-other-buffer '(helm-source-files-in-current-dir
                         helm-source-buffers-list
                         helm-source-buffer-not-found)
                       "*helm custom*")))
(define-key helm-command-map (kbd ";") 'helm-custom)

(use-package helm-projectile
  :commands (helm-browse-project helm-projectile-buffers)
  :config (progn
            (defun helm-projectile-buffers ()
              (interactive)
              (let ((helm-truncate-lines t))
                  (helm-other-buffer '(helm-source-projectile-buffers-list)
                                     "*helm project buffers*")))
            ;; to prevent overwriting
            (define-key projectile-command-map [?h] 'helm-browse-project)))

(helm-mode 1)

;;; Do not remove, used for the interactive evaluation!
;; (setq helm-completing-read-handlers-alist nil)

;;; use ido instead of helm
(dolist (command
         '(switch-to-buffer
           c-set-offset
           unload-feature
           org-agenda-filter-by-tag
           org-match-sparse-tree))
  (add-to-list 'helm-completing-read-handlers-alist
               (cons command 'ido)))

;;; use completing-read instead of helm
(dolist
    (command
     '(TeX-command-master
       LaTeX-section
       LaTeX-environment
       insert-char
       sql-get-login
       find-file-at-point
       highlight-regexp
       org-set-tags
       magit-gitignore
       execute-extended-command
       find-file
       ff-find-other-file))
  (add-to-list 'helm-completing-read-handlers-alist
               (cons command nil)))

(provide 'my-helm)
