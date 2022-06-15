;;; -*- lexical-binding: t; -*-
(require 'early-init (expand-file-name "early-init" user-emacs-directory))

(use-package no-littering :straight t)

(add-to-list 'load-path (expand-file-name "vendor/" user-emacs-directory))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;;; Load all the config parts.
(require 'config-lib (expand-file-name "config-lib" user-emacs-directory))
(load-numbered-parts (expand-file-name "lisp/" user-emacs-directory))
(require-parts (no-littering-expand-etc-file-name "skeletons/"))

;;; Load the machine-local config parts.
(unless (getenv "EMACS_NO_LOCAL")
  (let ((local-lisp (expand-file-name "local" user-emacs-directory)))
    (load local-lisp 'noerror))
  (let ((local-lisps (expand-file-name "local.d/" user-emacs-directory)))
    (when (file-directory-p local-lisps)
      (load-parts local-lisps))))
