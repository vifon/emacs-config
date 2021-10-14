;;; -*- lexical-binding: t; -*-

(add-hook 'c-mode-common-hook
          (defun my-c-common-hook ()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            (hs-minor-mode 1)))
(add-hook 'c++-mode-hook
          (lambda ()
            (setq-local c-macro-cppflags "-x c++")))


(add-hook 'gud-mode-hook (lambda () (setq tab-width 8)))


(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))

(add-hook 'semantic-symref-results-mode-hook
          (lambda () (toggle-read-only 1)))

(mapc (lambda (mode)
        (add-hook mode (lambda ()
                         (setq tex-open-quote ",,")
                         (setq TeX-open-quote ",,")
                         (visual-line-mode 1))))
      '(latex-mode-hook
        LaTeX-mode-hook))

(add-hook 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)
