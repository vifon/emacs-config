(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-view-program-list (quote (("Evince" "evince %o") ("Zathura" "zathura %o"))))
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "Evince")
     (output-html "xdg-open"))))
 '(bmkp-last-as-first-bookmark-file "~/.emacs.bmk")
 '(calendar-date-style (quote european))
 '(calendar-latitude 52.26)
 '(calendar-longitude 21.02)
 '(calendar-mark-diary-entries-flag nil)
 '(calendar-mark-holidays-flag nil)
 '(calendar-week-start-day 1)
 '(cc-search-directories
   (quote
    ("." "/usr/include" "/usr/local/include/*" "/home/vifon/local/include")))
 '(ediff-grab-mouse nil)
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(flymake-gui-warnings-enabled t)
 '(glasses-separate-parentheses-p nil)
 '(httpd-port 38080)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(rainbow-x-colors-major-mode-list
   (quote
    (emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode sh-mode)))
 '(recentf-exclude (quote ("^/media/" "COMMIT_EDITMSG$" "^mutt-")))
 '(recentf-max-menu-items 10000)
 '(recentf-max-saved-items 1000)
 '(safe-local-variable-values
   (quote
    ((eval ispell-change-dictionary "pl")
     (eval flyspell-buffer))))
 '(server-raise-frame nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((((background dark)) (:foreground "green"))))
 '(diff-changed ((((background dark)) (:foreground "yellow"))))
 '(diff-context ((((background dark)) (:foreground "White")) (t (:foreground "Black"))))
 '(diff-file-header ((((background dark)) (:foreground "Cyan" :background "Black")) (t (:foreground "Red" :background "White"))))
 '(diff-header ((((background dark)) (:foreground "Cyan")) (t (:foreground "Red"))))
 '(diff-hl-change ((t (:background "yellow3" :foreground "yellow2"))))
 '(diff-hl-delete ((t (:background "firebrick" :foreground "red"))))
 '(diff-hl-insert ((t (:background "lime green" :foreground "green"))))
 '(diff-hunk-header ((((background dark)) (:foreground "Black" :background "#05057F7F8D8D")) (t (:foreground "White" :background "Salmon"))))
 '(diff-index ((((background dark)) (:foreground "Magenta")) (t (:foreground "Green"))))
 '(diff-indicator-added ((t (:inherit diff-added))))
 '(diff-indicator-changed ((t (:inherit diff-changed))))
 '(diff-indicator-removed ((t (:inherit diff-removed))))
 '(diff-nonexistent ((((background dark)) (:foreground "#FFFFFFFF7474")) (t (:foreground "DarkBlue"))))
 '(diff-refine-added ((((background dark)) (:inherit diff-added :background "green4")) (t (:inherit diff-added :background "#88ee88"))))
 '(diff-refine-removed ((((background dark)) (:inherit diff-removed :background "red4")) (t (:inherit diff-removed :background "#ff9999"))))
 '(diff-removed ((((background dark)) (:foreground "red"))))
 '(notmuch-search-unread-face ((t (:weight extra-bold))))
 '(term-color-blue ((t (:background "#4276b8" :foreground "#4276b8"))))
 '(term-color-cyan ((t (:background "#0b939b" :foreground "#0b939b"))))
 '(term-color-green ((t (:background "#00cc00" :foreground "#00cc00"))))
 '(term-color-magenta ((t (:background "#95709b" :foreground "#95709b"))))
 '(term-color-red ((t (:background "#dc0000" :foreground "#dc0000"))))
 '(term-color-yellow ((t (:background "#c4a000" :foreground "#c4a000")))))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'set-goal-column 'disabled nil)
