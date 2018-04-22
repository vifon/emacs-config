;; -*- lexical-binding: t; -*-

(defun yank-pop-dwim (fun)
  "Return a function calling `fun' unless the last command was
`yank'. If the last command was `yank', it will call `yank-pop'
instead to emulate the default Emacs behavior."
  (lambda (arg)
    (interactive "P")
    (if (equal last-command 'yank)
        (yank-pop arg)
      (funcall fun))))

(use-package smex :ensure t :defer t)
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)
         ("C-c h" . counsel-hydra/body)
         ("M-y" . counsel-yank-pop-dwim)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable))
  :init (fset 'counsel-yank-pop-dwim
              (yank-pop-dwim #'counsel-yank-pop))
  :config (setq ivy-use-selectable-prompt t))

(use-package ivy-hydra :ensure t :defer t)
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-x M-r" . ivy-resume)
         ("C-x M-f" . find-file-default)
         :map ivy-minibuffer-map
         ("C-c C-f" . ivy-toggle-calling)
         ("C-c C-m" . ivy-toggle-fuzzy)
         ("C-c o" . ivy-done-other-window))
  :init (progn
          (ivy-mode 1)
          (require 'ivy-hydra)
          (defun ivy-done-other-window ()
            (interactive)
            (-if-let* ((actions (ivy-state-action ivy-last))
                       (other-window-action-tuple (assoc "j" actions))
                       (other-window-action (second other-window-action-tuple)))
                (ivy-set-action other-window-action))
            (ivy-done))
          (defun find-file-default ()
            "Call find-file with the default completion system."
            (interactive)
            (let ((completing-read-function #'completing-read-default)
                  (completion-in-region-function #'completion--in-region))
              (call-interactively #'find-file)))))

(use-package swiper
  :ensure t
  :bind (("C-c f" . swiper)
         ("M-s f" . swiper-symbol-at-point)
         :map isearch-mode-map
         ("C-i" . swiper-from-isearch))
  :commands (swiper-from-isearch)
  :init (defun swiper-symbol-at-point ()
          (interactive)
          (swiper (thing-at-point 'symbol 'no-properties))))

(defhydra counsel-hydra
            (:color blue)
            ("h" counsel-git "git-files")
            ("H" counsel-rg "ripgrep")
            ("r" ivy-recentf "recentf"))

(provide 'my-ivy)
