(require 'helm-config)
(use-package helm-mode
  :bind (:map helm-map
         ("C-;" . helm-execute-persistent-action)
         ("C-i" . helm-execute-persistent-action)
         ("C-z" . helm-select-action))
  :diminish helm-mode)
(use-package helm-ls-git
  :defer t)
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

(global-set-key (kbd "M-X")      #'helm-M-x)
(global-set-key (kbd "C-h C-f")  #'helm-apropos)
(global-set-key (kbd "M-s f")    #'helm-swoop)
(global-set-key [remap find-tag] #'helm-etags-select)

(defun my-helm-browse-project--basename (arg)
  (interactive "p")
  (let ((helm-ff-transformer-show-only-basename t))
    (helm-browse-project arg)))
(global-set-key (kbd "C-c H H") #'my-helm-browse-project--basename)

(setq helm-etags-execute-action-at-once-if-one nil)

(defun helm-custom ()
  (interactive)
  (require 'helm-files)
  (let ((helm-ff-transformer-show-only-basename t))
    (helm-other-buffer '(helm-source-files-in-current-dir
                         helm-source-buffers-list
                         helm-source-buffer-not-found)
                       "*helm custom*")))
(eval-after-load "helm.el"
  '(define-key helm-command-map (kbd ";") 'helm-custom))

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

;; (helm-mode 1)

;;; Do not remove, used for the interactive evaluation!
;; (setq helm-completing-read-handlers-alist nil)

(provide 'my-helm)
