;; -*- lexical-binding: t; -*-

(use-package selectrum
  :ensure t
  :bind (("C-x M-r" . selectrum-repeat)
         :map selectrum-minibuffer-map
         ("C-l" . backward-kill-sexp))
  :init (selectrum-mode 1)
  :config (progn
            (autoload 'ffap-file-at-point "ffap")
            (add-hook 'completion-at-point-functions
                      (defun complete-path-at-point+ ()
                        (let ((fn (ffap-file-at-point))
                              (fap (thing-at-point 'filename)))
                          (when (and (or fn
                                         (equal "/" fap))
                                     (save-excursion
                                       (search-backward fap (line-beginning-position) t)))
                            (list (match-beginning 0)
                                  (match-end 0)
                                  #'completion-file-name-table))))
                      'append)))

(use-package selectrum-prescient
  :ensure t
  :after selectrum
  :config (progn
            (setq selectrum-prescient-enable-filtering nil)
            (selectrum-prescient-mode 1)))

(use-package prescient
  :ensure t
  :defer t
  :config (prescient-persist-mode 1))

(use-package orderless
  :ensure t
  :after selectrum
  :config (progn
            (setq orderless-matching-styles '(orderless-regexp
                                              orderless-initialism
                                              orderless-prefixes)
                  orderless-skip-highlighting (lambda () selectrum-is-active)
                  selectrum-refine-candidates-function #'orderless-filter
                  selectrum-highlight-candidates-function #'orderless-highlight-matches
                  completion-styles '(orderless))

            (defun vifon/orderless-without-if-bang (pattern index total)
              (when (string-prefix-p "!" pattern)
                `(orderless-without-literal . ,(substring pattern 1))))
            (defun vifon/orderless-literal-if-equal (pattern index total)
              (when (string-suffix-p "=" pattern)
                `(orderless-literal . ,(substring pattern 0 -1))))
            (setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                                vifon/orderless-literal-if-equal))))

(use-package embark
  :ensure t
  :bind (("C-c o" . embark-act)
         :map minibuffer-local-map
         ("M-o"   . embark-act))
  :config (setf (alist-get 'project-file embark-keymap-alist)
                (embark-define-keymap embark-project-map
                  "Keymap for Embark project actions."
                  ("k" project-remove-known-project))))

(use-package embark-consult
  :ensure t
  :after (embark consult))

(use-package marginalia
  :ensure t
  :after selectrum
  :demand t                     ; :demand applies to :bind but not
                                ; :after.  We want to eagerly load
                                ; marginalia once selectrum is loaded.
  :bind (:map minibuffer-local-map
         ("C-o" . marginalia-cycle))
  :config (marginalia-mode 1))

(use-package consult
  :ensure t
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

(provide 'my-completing-read)
