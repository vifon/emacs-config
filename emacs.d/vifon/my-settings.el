(global-subword-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)

(setq tags-revert-without-query t
      tags-add-tables nil)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(setq bidi-display-reordering nil)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode 0))
(when (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'tooltip-mode) (tooltip-mode 0))
(column-number-mode t)

(setq show-paren-delay 0)
(setq echo-keystrokes 0.1)
(show-paren-mode t)

(blink-cursor-mode 0)
(setq scroll-step 10
      next-screen-context-lines 10)
(mouse-avoidance-mode 'none)
(transient-mark-mode 1)

(setq backup-directory-alist '(("^/media/" . ".")
                               ("." . "~/.emacs_saves"))
      backup-by-copying t)

(setq vc-follow-symlinks t)
(when (equal system-type 'gnu/linux)
  (setq x-select-enable-primary t
        select-active-regions nil
        mouse-drag-copy-region t
        x-select-enable-clipboard t))

(setq iswitchb-default-method 'samewindow)

(setq compile-command "make -k -C .")
(setq gud-gdb-command-name "gdb -q -i=mi")

(setq inferior-octave-startup-args '("-q"))

(setq tramp-default-method "sshx")

;; Turn off tabs
(setq-default indent-tabs-mode nil)

;; Set the tab width
(setq default-tab-width 4
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

(setq compilation-scroll-output 'first-error
      compilation-read-command nil)

(setq shell-file-name "/bin/sh")

(setq find-file-existing-other-name nil)
(setq require-final-newline t)
(setq-default indicate-buffer-boundaries 'left)

(setq reb-re-syntax 'string)

(require 'cl)
(setq tab-stop-list (loop for i from 4 to 480 by 4 collecting i))

(when (version<= "24.4" emacs-version)
  (setq browse-url-browser-function #'browse-url-generic)
  (setq shr-external-browser 'browse-url-generic)
  (setq eww-download-directory "~/dl"))
(setq browse-url-generic-program "webbroboard")

(add-to-list 'auto-mode-alist '("^/tmp/mutt.*" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.claws-mail/tmp/tmpmsg\\.0x.*" . mail-mode))

(setq epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'\\|\\.asc")
(epa-file-name-regexp-update)

;; My window managers usually don't play nice with iconifying Emacs.
;; Let's disable it but save the original function under a different
;; name.
(fset 'iconify-orig (symbol-function 'iconify-frame))
(defalias 'iconify-frame 'ignore)

(defalias 'elpa 'list-packages)
(defalias 'repl 'ielm)
(defalias 'colors 'list-colors-display)
(defalias 'desktop-load 'desktop-change-dir)
(setq desktop-load-locked-desktop t)


(provide 'my-settings)
