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
  :init (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch))

(setq helm-locate-command "locate %s -e -A %s")
(setq helm-buffer-max-length 40)

(global-set-key [remap occur]                    'helm-occur)
(global-set-key (kbd "M-X")                      'smex)
(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap yank-pop]                 'helm-show-kill-ring)
(global-set-key (kbd "C-h C-f")                  'helm-apropos)
(global-set-key (kbd "C-x b")                    'helm-buffers-list)
(global-set-key (kbd "M-s f")                    'helm-swoop)
(global-set-key [remap find-tag]                 'helm-etags-select)

(key-chord-define-global "`h"
                         (defun my-helm-browse-project--basename (arg)
                           (interactive "p")
                           (let ((helm-ff-transformer-show-only-basename t))
                             (helm-browse-project arg))))

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

(provide 'my-helm)
