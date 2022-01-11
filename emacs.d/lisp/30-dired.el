;;; -*- lexical-binding: t; -*-

(use-package dired
  :bind (:map dired-mode-map
              ("z" . dired-subtree-toggle)
              ("TAB" . vifon/dired-transient)
              ("I" . vifon/dired-insert-subdir-keep-point)
              ("* C" . vifon/dired-change-marks*)
              ("E" . vifon/dired-dragon))
  :init (progn
          (defun vifon/dired-insert-subdir-keep-point ()
            (interactive)
            (let ((dir-name (dired-get-filename t)))
              (call-interactively #'dired-maybe-insert-subdir)
              (dired-jump)
              (dired-next-line 1)
              (message "Inserted `%s'" dir-name)))

          (defun vifon/dired-change-marks* (&optional new)
            (interactive
             (let* ((cursor-in-echo-area t)
                    (new (progn (message "Change * marks to (new mark): ")
                                (read-char))))
               (list new)))
            (dired-change-marks ?* new))

          (defun vifon/dired-dragon (&optional single)
            (interactive "P")
            (dired-do-async-shell-command (if single
                                              "dragon -x *"
                                            "dragon -a -x *")
                                          nil
                                          (dired-get-marked-files))))
  :config (progn
            (setq dired-dwim-target nil
                  dired-free-space-args "-Pkh"
                  dired-ls-F-marks-symlinks t
                  dired-isearch-filenames 'dwim
                  dired-omit-files "^\\.?#\\|^\\.[^\\.]\\|^\\.\\.."
                  wdired-allow-to-change-permissions t
                  image-dired-external-viewer "sxiv")

            (setq dired-listing-switches "-alh --group-directories-first -v")))

(use-package dired-x
  :bind (("C-x C-j" . dired-jump)
         :map dired-mode-map
         ("h" . dired-jump))
  :init (setq dired-x-hands-off-my-keys t))

(use-package dired-subtree
  :straight t
  :after dired
  :commands dired-subtree-toggle)

(use-package dired-filter
  :straight t
  :after dired
  :config (setq-default dired-filter-stack '()))

(use-package dired-rifle
  :straight t
  :after dired)

(use-package dired-collapse
  :straight t
  :after dired)

(use-package dired-async
  :straight async
  :after dired
  :commands dired-async-mode
  :config (setq dired-async-message-function
                (lambda (text face &rest args)
                  (call-process "notify-send" nil 0 nil
                                "Emacs: dired-async"
                                (apply #'format text args))
                  (apply #'dired-async-mode-line-message text face args))))
