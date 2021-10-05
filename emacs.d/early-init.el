;;; -*- lexical-binding: t; -*-
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold))
          'append)
(setq gc-cons-threshold most-positive-fixnum)

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(defun bootstrap-straight ()
  (setq straight-vc-git-default-clone-depth 'full
        straight-check-for-modifications '(check-on-save find-when-checking)
        straight-build-dir (format "build-%s" emacs-version))
  (defvar bootstrap-version)
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun bootstrap-use-package ()
  (setq use-package-enable-imenu-support t)
  (straight-use-package 'use-package)
  (use-package diminish :straight t :defer t))

(bootstrap-straight)
(bootstrap-use-package)

(dolist (mode '(scroll-bar-mode
                horizontal-scroll-bar-mode
                menu-bar-mode
                tool-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))


(when (file-exists-p "~/.emacs.d/early-local.el")
  (load "~/.emacs.d/early-local.el"))

(provide 'early-init)
