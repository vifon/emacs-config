;; -*- lexical-binding: t; -*-

(use-package smex :ensure t :defer t)
(use-package counsel
  :ensure t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x M-f" . find-file-default)
         ("C-x C-r" . counsel-recentf)
         ("C-c h" . counsel-hydra/body)
         ("M-y" . counsel-yank-pop-dwim)
         ([remap describe-function] . counsel-describe-function)
         ([remap describe-variable] . counsel-describe-variable)
         ("C-x 8 C-<return>" . counsel-unicode-char))
  :init (progn
          (defun counsel-yank-pop-dwim (arg)
            "Emulate the original `yank-pop' behavior."
            (interactive "P")
            (if (equal last-command 'yank)
                (yank-pop arg)
              (counsel-yank-pop)))
          (defun find-file-default ()
            "Call find-file with the default completion system."
            (interactive)
            (let ((completing-read-function #'completing-read-default)
                  (completion-in-region-function #'completion--in-region))
              (call-interactively #'find-file)))))

(use-package ivy-hydra :ensure t :defer t)
(use-package ivy
  :ensure t
  :diminish ivy-mode
  :bind (("C-x M-r" . ivy-resume)
         :map ivy-minibuffer-map
         ("C-c C-f" . ivy-toggle-calling)
         ("C-c C-m" . ivy-toggle-fuzzy))
  :init (progn
          (setq ivy-do-completion-in-region nil)
          (ivy-mode 1)

          (require 'ivy-hydra)
          (setq ivy-use-virtual-buffers t))
  :config (progn
            (setq ivy-use-selectable-prompt t)
            (add-to-list 'ivy-completing-read-handlers-alist
                         '(dired-diff . completing-read-default))))

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
            ("l" counsel-locate "locate")
            ("r" ivy-recentf "recentf"))

(provide 'my-ivy)
