;;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :bind (:map vertico-map
         ("C-l" . vertico-directory-delete-word))
  :init (vertico-mode 1))

(use-package vertico-repeat
  :after vertico
  :bind ("C-x M-r" . vertico-repeat)
  :config (add-hook 'minibuffer-setup-hook #'vertico-repeat-save))
(use-package vertico-mouse
  :after vertico
  :config (vertico-mouse-mode 1))
(use-package vertico-multiform
  :after vertico
  :init (vertico-multiform-mode 1)
  :bind (:map vertico-map
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat)))

(use-package orderless
  :straight t
  :after vertico
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  completion-styles '(orderless))

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration/nix".  The "basic" style is
            ;; needed to support the hostname completion in the TRAMP
            ;; inputs such as "/sshx:HOSTNAME".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion))))

            (defun vifon/orderless-without-if-bang (pattern index total)
              (when (string-prefix-p "!" pattern)
                `(orderless-without-literal . ,(substring pattern 1))))
            (defun vifon/orderless-literal-if-equal (pattern index total)
              (when (string-suffix-p "=" pattern)
                `(orderless-literal . ,(substring pattern 0 -1))))
            (setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                                vifon/orderless-literal-if-equal))))

(use-package embark
  :straight t
  :bind (("C-c o" . embark-act)
         ("C-."   . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act))
  :config (setq embark-mixed-indicator-delay 2))

(use-package embark-consult
  :straight t
  :after (embark consult))

(use-package marginalia
  :straight t
  :after vertico
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once vertico is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :straight t
  :bind (("M-s f" . consult-line)
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g r" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line))
  :init (bind-key "TAB"
                  (lambda ()
                    (interactive)
                    (isearch-exit)
                    (consult-line isearch-string))
                  isearch-mode-map)
  :config (progn
            (setq consult-project-root-function #'vc-root-dir)
            (consult-customize
             consult-ripgrep consult-grep
             consult-buffer consult-recent-file
             :preview-key (kbd "M-."))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
                        (delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))))


;;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
(autoload 'ffap-file-at-point "ffap")
(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table :exclusive 'no))))
          'append)
