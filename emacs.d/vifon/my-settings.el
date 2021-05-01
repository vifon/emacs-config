(global-subword-mode 1)
(diminish 'subword-mode)
(electric-pair-mode 1)
(electric-indent-mode 1)

(setq tags-revert-without-query t
      tags-add-tables nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq bidi-display-reordering nil)

(tooltip-mode 0)

(column-number-mode t)

(when (version<= "26.1" emacs-version)
  (setq auto-hscroll-mode 'current-line))

(when (boundp 'async-shell-command-display-buffer)
  (setq async-shell-command-display-buffer nil))

(setq show-paren-delay 0)
(setq echo-keystrokes 0.1)
(show-paren-mode t)

(blink-cursor-mode 0)
(setq visible-cursor nil)
(setq scroll-conservatively 10
      next-screen-context-lines 10)
(mouse-avoidance-mode 'none)
(transient-mark-mode 1)

(setq display-buffer-alist '(("\\*Calendar\\*" display-buffer-below-selected)))

(setq backup-directory-alist '(("^/media/" . ".")
                               ("^/run/media/" . ".")
                               ("." . "~/.emacs_saves"))
      tramp-backup-directory-alist '(("." . "."))
      backup-by-copying t)

;;; Never timeout the TRAMP sessions, it causes Emacs to hang too
;;; often.  I'd rather manage their lifetime myself.  This timeout got
;;; added in Emacs 27.
(setq tramp-connection-properties '((nil "session-timeout" nil)))

(setq tramp-completion-use-auth-sources nil)

(setq vc-follow-symlinks t)
(when (equal system-type 'gnu/linux)
  (setq x-select-enable-primary t
        select-active-regions nil
        mouse-drag-copy-region t
        x-select-enable-clipboard t))

;; Turn off tabs
(setq-default indent-tabs-mode nil)

;; Set the tab width
(setq-default default-tab-width 4
              tab-width 4
              c-basic-indent 4
              c-basic-offset 4)
(use-package cc-vars
  :defer t
  :config (setf (cdr (assoc 'other
                            c-default-style))
                "k&r"))
(c-set-offset 'inline-open 0)
(c-set-offset 'access-label -2)
(c-set-offset 'innamespace 0)

(setq parens-require-spaces nil)

(setq require-final-newline t)
(setq-default indicate-buffer-boundaries 'left)

(setq password-cache-expiry 300)

(when (version<= "24.4" emacs-version)
  (setq browse-url-browser-function #'browse-url-generic)
  (if (version<= "27.0" emacs-version)
      (setq browse-url-secondary-browser-function 'browse-url-generic)
    (setq shr-external-browser 'browse-url-generic))
  (setq eww-download-directory "~/dl"))
(setq browse-url-generic-program "webbroboard")

(add-to-list 'auto-mode-alist '("^/tmp/mutt.*" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.claws-mail/tmp/tmpmsg\\.0x.*" . mail-mode))

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("/Pkgfile\\'" . sh-mode))

(setq epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'\\|\\.asc")
(epa-file-name-regexp-update)

;; My window managers usually don't play nice with iconifying Emacs.
;; Let's disable it but save the original function under a different
;; name.
(fset 'iconify-orig (symbol-function 'iconify-frame))
(defalias 'iconify-frame 'ignore)

(defalias 'elpa 'package-list-packages)


(provide 'my-settings)
