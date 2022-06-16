;;; -*- lexical-binding: t; -*-

;;; Some snake oil during the startup.  Probably won't hurt and is
;;; reverted right after the Emacs initialization finishes.
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold ,gc-cons-threshold))
          'append)
(setq gc-cons-threshold most-positive-fixnum)

(setq warning-minimum-level :error)

;; Workaround for https://debbugs.gnu.org/34341 in GNU Emacs <= 26.3.
(when (and (version< emacs-version "26.3") (>= libgnutls-version 30603))
  (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(defun bootstrap-straight ()
  "Install and configure straight.el"
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
  "Install use-package.el"
  (setq use-package-enable-imenu-support t)
  (straight-use-package 'use-package)
  (use-package diminish :straight t :defer t))


;;; Load the machine-local config.
(unless (getenv "EMACS_NO_LOCAL")
  (when (file-exists-p "~/.emacs.d/early-local.el")
    (load "~/.emacs.d/early-local.el")))

;;; Bootstrap the packages used to manage the rest of this config.
(bootstrap-straight)
(bootstrap-use-package)

;;; Disable the GUI elements I don't use.
(dolist (mode '(scroll-bar-mode
                horizontal-scroll-bar-mode
                menu-bar-mode
                tool-bar-mode))
  (when (fboundp mode)
    (funcall mode 0)))

;;; This is not needed in general, but my specific init.el tries to
;;; explicitly load early-init.el if it wasn't loaded yet.
;;; This `provide' is used to load it only once.
(provide 'early-init)
