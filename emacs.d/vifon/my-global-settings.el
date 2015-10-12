(global-subword-mode 1)
(electric-pair-mode 1)
(electric-indent-mode 1)

(setq-default fill-column 70)
(setq tags-revert-without-query t)
(setq initial-major-mode 'org-mode)
(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

(scroll-bar-mode 0)
(when (version<= "25.0" emacs-version)
  (horizontal-scroll-bar-mode 0))
(tool-bar-mode 0)
(menu-bar-mode 0)
(column-number-mode t)

(setq show-paren-delay 0)
(setq echo-keystrokes 0.1)
(show-paren-mode t)

(blink-cursor-mode 0)
(setq scroll-step 10
      next-screen-context-lines 10)
(mouse-avoidance-mode 'none)
(transient-mark-mode 1)

(setq cua-enable-cua-keys nil)
(require 'cualess-global-mark)
(setq cua-global-mark-keep-visible nil)

(setq backup-directory-alist '(("." . "~/.emacs_saves"))
      backup-by-copying t)

(setq vc-follow-symlinks t)
(when (equal system-type 'gnu/linux)
  (setq x-select-enable-primary t
        select-active-regions nil
        mouse-drag-copy-region t
        x-select-enable-clipboard nil))

(setq iswitchb-default-method 'samewindow)

(setq epa-armor t)

(setq compile-command "make -k -C .")
(setq gud-gdb-command-name "gdb -q -i=mi")

(setq inferior-octave-startup-args '("-q"))

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
(setq dired-listing-switches "-alhvF --group-directories-first")
(setq dired-ls-F-marks-symlinks t)
(setq dired-omit-files "^\\.?#\\|^\\.[^\\.].*")
(setq dired-isearch-filenames 'dwim)

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
(setq browse-url-generic-program "chromium")

(setf (cdr (assoc "\\.pdf\\'"
                  org-file-apps))
      "evince %s")

(add-to-list 'auto-mode-alist '("^/tmp/mutt.*" . mail-mode))
(add-to-list 'auto-mode-alist '("\\.claws-mail/tmp/tmpmsg\\.0x.*" . mail-mode))

(setq epa-file-name-regexp "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'\\|\\.asc")
(epa-file-name-regexp-update)

(defalias 'elpa 'list-packages)
(defalias 'repl 'ielm)
(defalias 'colors 'list-colors-display)
(defalias 'desktop-load 'desktop-change-dir)
(setq desktop-load-locked-desktop t)


(provide 'my-global-settings)
