;; -*- lexical-binding: t; -*-

(defun yank-pop-dwim (fun)
  (lambda (arg)
    (interactive "P")
    (if (equal last-command 'yank)
        (yank-pop arg)
      (funcall fun))))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c h h" . counsel-git)
         ("C-c h H" . counsel-ag)
         ("M-y" . counsel-yank-pop-dwim))
  :init (fset 'counsel-yank-pop-dwim
              (yank-pop-dwim #'counsel-yank-pop)))

(use-package ivy
  :bind (("C-x C-r" . ivy-resume)
         ("C-x M-r" . ivy-recentf)
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
            (ivy-done))))

(use-package swiper
  :bind (("C-c f" . swiper)
         :map isearch-mode-map
         ("C-i" . swiper-from-isearch))
  :commands (swiper-from-isearch))

(provide 'my-ivy)
