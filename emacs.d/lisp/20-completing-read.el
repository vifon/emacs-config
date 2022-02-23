;;; -*- lexical-binding: t; -*-

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*"))
  :bind (("C-x M-r" . vertico-repeat)
         :map vertico-map
         ("C-l" . vertico-directory-delete-word)
         ("M-g" . vertico-multiform-grid)
         ("M-q" . vertico-multiform-flat))
  :init (vertico-mode 1)
  :config (progn
            (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
            (vertico-mouse-mode 1)
            (vertico-multiform-mode 1)
            (setq vertico-multiform-categories '((consult-grep buffer))
                  vertico-multiform-commands '((tmm-menubar flat)
                                               (tmm-shortcut flat)))))

(use-package orderless
  :straight t
  :after vertico
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-component-separator #'orderless-escapable-split-on-space)

            ;; Use the built-in "partial-completion" style to complete
            ;; file inputs such as "/e/ni/co.nix" into
            ;; "/etc/nixos/configuration.nix".  The "basic" style is
            ;; needed to support the hostname completion in the TRAMP
            ;; inputs such as "/sshx:HOSTNAME".
            (setq completion-category-defaults nil
                  completion-category-overrides '((file (styles basic partial-completion))))

            (setq completion-styles '(orderless))

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
  :config (progn
            (setq embark-mixed-indicator-delay 2)

            (cl-pushnew 'embark--allow-edit
                        (alist-get 'pp-eval-expression embark-target-injection-hooks))

            ;; Reload the project list after using
            ;; C-u `embark-act' with `project-forget-project'.
            (cl-pushnew 'embark--restart
                        (alist-get 'project-forget-project embark-post-action-hooks))

            (defun embark-act-with-eval (expression)
              "Evaluate EXPRESSION and call `embark-act' on the result."
              (interactive "sExpression: ")
              (with-temp-buffer
                (insert (eval (read expression)))
                (embark-act)))

            (dolist (keymap (list embark-variable-map embark-expression-map))
              (bind-key "v" #'embark-act-with-eval
                        keymap))

            ;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
            (autoload 'gnus-dired-attach "gnus-dired" nil t)
            (defun embark-attach-file (file)
              "Attach FILE to an email message."
              (interactive "fAttach: ")
              (gnus-dired-attach (list file)))
            (bind-key "a" #'embark-attach-file embark-file-map)))

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
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ([remap previous-matching-history-element] . consult-history))
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

            (defun vifon/orderless-fix-consult-tofu (pattern index total)
              "Ignore the last character which is hidden and used only internally."
              (when (string-suffix-p "$" pattern)
                `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                              "[\x100000-\x10FFFD]*$"))))

            (dolist (command '(consult-buffer consult-line))
              (advice-add command :around
                          (lambda (orig &rest args)
                            (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                                     orderless-style-dispatchers)))
                              (apply orig args)))))

            ;; Disable consult-buffer project-related capabilities as
            ;; they are very slow in TRAMP.
            (setq consult-buffer-sources
                  (delq 'consult--source-project-buffer
                        (delq 'consult--source-project-file consult-buffer-sources)))

            (setq consult--source-hidden-buffer
                  (plist-put consult--source-hidden-buffer :narrow ?h))

            (advice-add #'completing-read-multiple :override
                        #'consult-completing-read-multiple)))


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

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Use the completing-read UI for the M-tab completion.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
(setq completion-cycle-threshold 3)
