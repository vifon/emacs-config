(defgroup scratch-mode nil
  "A specialized mode for the scratch buffer.")

(defun scratch-reset ()
  (interactive)
  (switch-to-buffer "*scratch*")
  (scratch-mode))

(defvar scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "o") #'org-mode)
    (define-key map (kbd "e") #'lisp-interaction-mode)
    (define-key map (kbd "m") #'markdown-mode)
    (define-key map (kbd "i") (lambda ()
                                (interactive)
                                (find-file "~/.emacs.d/init.el")))
    (define-key map (kbd "I") (lambda ()
                                (interactive)
                                (find-file "~/.emacs.d/early-init.el")))
    (define-key map (kbd "C") (lambda ()
                                (interactive)
                                (find-library "scratch-mode")))
    (define-key map (kbd "p") #'project-switch-project)
    (define-key map (kbd "P") #'package-list-packages)
    (define-key map (kbd "SPC") #'notmuch)
    (define-key map (kbd "s") #'notmuch-search)
    (define-key map (kbd "M") #'notmuch-mua-new-mail)
    (define-key map (kbd "z") #'deft)
    (define-key map (kbd "C-c C-x C-j") #'org-clock-goto)
    (define-key map (kbd "j") #'org-clock-goto)
    (define-key map (kbd "J") #'org-mru-clock-select-recent-task)
    (define-key map (kbd "C-'") #'org-cycle-agenda-files)
    (define-key map (kbd "C-c C-w") #'org-refile)
    (define-key map (kbd "a") #'org-agenda)
    (define-key map (kbd "A") #'org-agenda)
    (define-key map (kbd "r") (lookup-key global-map (kbd "C-x r")))
    (define-key map (kbd "g") #'scratch-reset)
    (define-key map (kbd "S") #'scratch-dir)
    map))

(defcustom scratch-mode-key-hints
  '("o"
    "e"
    "m"
    ("i" . "~/.emacs.d/init.el")
    ("I" . "~/.emacs.d/early-init.el")
    "p"
    "SPC"
    "s"
    "M"
    "z"
    "a"
    "j"
    "J"
    "S")
  "The keymap hints to show in `scratch-mode'."
  :type '(repeat string))

(define-derived-mode scratch-mode special-mode "scratch"
  "A dedicated scratch buffer mode with commonly used commands bound."
  (setq font-lock-defaults
        '((("^\\(.*?\\):" 1 'bold)
           (": \\(.*\\)$" 1 'italic))))

  (setq cursor-type nil)

  (emacs-lock-mode 'kill)
  (cd "~/")

  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (elem scratch-mode-key-hints)
      (let* ((key (if (consp elem)
                      (car elem)
                    elem))
             (desc (if (consp elem)
                       (cdr elem)
                     (local-key-binding (kbd key)))))
        (insert (format "%s: %s\n" key desc)))))
  (set-buffer-modified-p nil)
  (goto-char (point-min))
  (add-hook 'change-major-mode-hook
            (lambda ()
              (read-only-mode 0)
              (erase-buffer))
            nil t))

(provide 'scratch-mode)
