;;; -*- lexical-binding: t; -*-
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
        (setq package-user-dir (locate-user-emacs-file
                                (concat
                                 (file-name-as-directory "elpa")
                                 emacs-version)))
        (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
        (setq package-archives
              '(("gnu"          . "https://elpa.gnu.org/packages/")
                ("melpa"        . "https://melpa.org/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("org"          . "https://orgmode.org/elpa/"))
              package-archive-priorities
              '(("org"          . 20)
                ("melpa"        . 15)
                ("gnu"          . 10)
                ("melpa-stable" . 5)))
        (setq package-enable-at-startup nil)
        (package-initialize)
        (unless (package-installed-p 'use-package)
          (package-refresh-contents)
          (package-install 'use-package))
        (setq use-package-enable-imenu-support t)
        (require 'use-package))
    (message "WARNING: Ancient emacs! No advice-add, package.el")
    (defmacro advice-add (&rest body))
    (defmacro use-package (&rest body)))
  (use-package diminish :ensure t :defer t)
  (use-package bind-key :ensure t :defer t))
(bootstrap-use-package)

(dolist (mode '(scroll-bar-mode
                horizontal-scroll-bar-mode
                menu-bar-mode
                tool-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

(provide 'early-init)
