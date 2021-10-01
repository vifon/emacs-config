(use-package org
  :straight t
  :defer t
  :bind (:map org-mode-map
         ("<C-tab>" . nil))
  :config (progn
            (bind-key "C-c C-1"
                      (lambda ()
                        (interactive)
                        (org-time-stamp-inactive '(16)))
                      org-mode-map)
            (bind-key [remap insert-file]
                      (lambda (arg)
                        (interactive "P")
                        (call-interactively (if arg
                                                #'vifon/org-insert-directory
                                              #'insert-file)))
                      org-mode-map)
            (setf (cdr (assoc "\\.pdf\\'"
                              org-file-apps))
                  "evince %s")
            (add-to-list 'org-file-apps '("\\.docx\\'" . "libreoffice %s"))
            (add-to-list 'org-file-apps '(directory . emacs))
            (dolist (ext '("png" "jpg" "jpeg"))
              (add-to-list 'org-file-apps (cons (concat "\\." ext "\\'")
                                                "sxiv %s")))
            (setq org-default-notes-file (concat org-directory "/inbox.org"))
            (plist-put org-format-latex-options :scale 2.0)
            (setq org-adapt-indentation t)))

(use-package org-contrib
  :straight t
  :defer t)

(use-package org-attach
  :after org
  :init (defun org-attach-scrot ()
          (interactive)
          (let* ((timestamp (format-time-string "%Y-%m-%d_%H%M-%s"))
                 (basename (concat "scrot-"
                                   timestamp
                                   ".png"))
                 (filepath (concat "/tmp/"
                                   basename)))
            (shell-command
             (concat "scrot -s -d2 "
                     filepath))
            (org-attach-attach filepath nil 'mv)
            (push (list (concat "file:"
                                (file-relative-name
                                 (expand-file-name basename
                                                   (org-attach-dir)))))
                  org-stored-links)))
  :config (setq org-attach-use-inheritance t
                org-attach-preferred-new-method 'ask))

(use-package org-clock
  :commands (org-clock-goto org-clocking-p)
  :config (setq org-clock-into-drawer t
                org-clock-out-remove-zero-time-clocks t))

(use-package org-mru-clock
  :straight t
  :defer t
  :config (advice-add #'org-mru-clock-select-recent-task :after
                      #'org-back-to-heading))


(defun org-insert-heading-empty-line-fix ()
  "Correctly surround the new org headings with empty lines.
By default the empty line after the new heading is not inserted
when using the `*-respect-content' commands."
  (save-excursion
    (when (org-previous-line-empty-p)
      (insert "\n"))))
(advice-add #'org-insert-heading-respect-content :after
            #'org-insert-heading-empty-line-fix)
(advice-add #'org-insert-todo-heading-respect-content :after
            #'org-insert-heading-empty-line-fix)

(defun vifon/org-resolve-clocks-with-calc (orig &rest args)
  (require 'cl-lib)
  (cl-letf (((symbol-function 'read-number)
             (lambda (prompt &optional default)
               (string-to-number
                (calc-eval (read-string prompt
                                        nil
                                        nil default))))))
    (apply orig args)))
(advice-add #'org-resolve-clocks :around
            #'vifon/org-resolve-clocks-with-calc)

(defun org-followup ()
  (interactive)
  (let ((link (org-store-link nil)))
    (org-insert-heading-respect-content)
    (end-of-line)
    (save-excursion
      (insert "\n")
      (indent-for-tab-command)
      (org-time-stamp-inactive '(16))
      (insert "\n")
      (indent-for-tab-command)
      (insert "Follow-up of: " link))))

(defun vifon/org-insert-directory (directory)
  "Insert `directory' as a list of org-mode links."
  (interactive "D")
  (let ((org-stored-links (vifon/dired-org-store-links directory))
        (prefix (concat (make-string (current-indentation)
                                     ? ;a space
                                     )
                        "- ")))
    (delete-horizontal-space)
    (org-insert-all-links nil prefix)
    (delete-blank-lines)))

(defun vifon/dired-org-store-links (directory)
  "Return a list of all files in `directory' as an org-mode link."
  (require 'org)
  (require 'ol)
  (require 'dash)
  (with-current-buffer (find-file-noselect directory)
    (save-excursion
      (goto-char (point-min))
      (let ((links))
        (while (not (eobp))
          (when-let ((file (ignore-error 'error
                             (dired-get-filename))))
            (push (list (concat "file:" file)
                        (-> file
                          file-name-nondirectory
                          file-name-sans-extension))
                  links))
          (forward-line 1))
        (nreverse links)))))


(require 'ol-notmuch nil 'noerror)
(use-package org-protocol :after org)
(use-package org-inlinetask :after org)

(setq org-hide-leading-stars nil)
(setq org-special-ctrl-a/e t)
(setq org-use-speed-commands t)
(setq org-ellipsis "[…]")
(setq org-indent-mode-turns-off-org-adapt-indentation nil)

(defun my-org-mode-hook ()
  (add-to-list (make-local-variable 'electric-pair-pairs)
               '(?$ . ?$)))
(add-hook 'org-mode-hook #'my-org-mode-hook)

;;; http://www.howardism.org/Technical/Emacs/orgmode-wordprocessor.html
(font-lock-add-keywords 'org-mode
                        '(("^ +\\(*\\) "
                           (0 (prog1 nil
                                (compose-region (match-beginning 1)
                                                (match-end 1)
                                                "•"))))))

(setq org-refile-targets '((org-agenda-files :tag . "PROJECT")
                           (org-agenda-files :tag . "CATEGORY")
                           (org-agenda-files :tag . "GROUP")
                           (org-agenda-files :level . 1)
                           (nil :tag . "PROJECT")
                           (nil :tag . "CATEGORY")
                           (nil :tag . "GROUP")
                           (nil :maxlevel . 2)))
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done nil)
(setq org-agenda-log-mode-items '(closed))
(setq org-log-done 'time)
(setq org-icalendar-combined-agenda-file "~/org/org.ics"
      org-icalendar-use-deadline '(event-if-todo)
      org-icalendar-use-scheduled '(event-if-todo)
      org-icalendar-timezone "Europe/Warsaw"
      org-export-with-tasks 'todo)
(setq org-export-with-toc nil
      org-html-validation-link nil)
(setq org-blank-before-new-entry '((heading . nil)
                                   (plain-list-item . auto)))
(setq org-tags-exclude-from-inheritance '("PROJECT" "ATTACH"))
(setq org-todo-keyword-faces '(("NEXT" . "Tomato")))
(setq org-enforce-todo-dependencies t)

(setq org-cycle-open-archived-trees nil)
(setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-location ".archive/%s_archive::")
(setq org-log-into-drawer t)
(setq org-log-reschedule 'time
      org-log-redeadline 'time)
(setq org-catch-invisible-edits 'smart)
(setq org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t)
(setq org-id-link-to-org-use-id 'use-existing)
(setq org-return-follows-link t)

(setq org-image-actual-width nil)

(setq org-confirm-elisp-link-function #'y-or-n-p)

(setq org-agenda-files (if (file-exists-p "~/org/.agenda-files")
                           "~/org/.agenda-files"
                         '("~/org/gtd.org")))

(bind-key "C-c c" #'org-capture)
(bind-key "C-c a" #'org-agenda)
(bind-key "C-c S" #'org-store-link)

(setq org-capture-templates
      '(("t" "task")

        ("tt" "simple task" entry (file "")
         "* TODO %?\n  %U\n  %a\n  %i")

        ("ti" "task from an issue tracker" entry (file "")
         "* TODO %?%a\n  %U\n  %i")

        ("ts" "sub-task" entry
         (function (lambda ()
                     (if (org-clocking-p)
                         (org-clock-goto)
                       (org-back-to-heading))))
         "* TODO %?\n  %U\n%(when (org-clocking-p) \"  %a\n\")  %i"
         :unnarrowed t)

        ("tb" "project issue" entry
         (file+headline (concat (or (vc-root-dir)
                                    (projectile-project-root))
                                "todo.org")
                        "Issues")
         "* TODO %?\n  %U\n  %a\n  %i")


        ("n" "task note" item (clock)
         "- %a%(unless (string-empty-p \"%i\") \" :: %i\")"
         :immediate-finish t)

        ("N" "note" entry (file "notes.org")
         "* %?  :NOTE:\n  %U\n  %a\n  %i")

        ("j" "journal" entry (file+olp+datetree "journal.org.gpg")
         "* %?\n  %i"
         :time-prompt t)

        ("J" "dev-diary" entry (file+olp+datetree "~/work.d/dev-diary.org")
         "* %?\n  %i"
         :time-prompt t)

        ("p" "purchase" entry (file "purchases.org")
         "* %?%a\n  %U\n  %i")

        ("m" "mail")

        ("mm" "meeting" entry (file "")
         "* TODO %?%a\n  SCHEDULED: %^T\n  %U\n  %i")))

(setq org-capture-templates-contexts
      '(("m" ((in-mode . "notmuch-show-mode")))
        ("ts" ((lambda () (or (org-clocking-p)
                              (derived-mode-p 'org-mode)))))
        ("tb" ((lambda () (and (derived-mode-p 'prog-mode)
                               (or (vc-root-dir)
                                   (projectile-project-root))))))
        ("n" ((lambda () (org-clocking-p))))
        ("n" "N" ((lambda () (not (org-clocking-p)))))))


(defun vifon/truncate-org-mode-line ()
  (let* ((heading-text (nth 4 (org-heading-components)))
         (text-without-links
          (if (string-prefix-p "[[" heading-text)
              (replace-regexp-in-string ".*\\]\\[\\(.*\\)\\]"
                                        "\\1" heading-text)
            heading-text))
         (max-length 10))
    (replace-regexp-in-string (concat "\\(.\\{"
                                      (number-to-string max-length)
                                      "\\}[^[:space:]]*\\).*")
                              "\\1…" text-without-links)))
(setq org-clock-heading-function #'vifon/truncate-org-mode-line)

(setq org-speed-commands-user
      '(("z" . org-kill-note-or-show-branches)))

(setq org-stuck-projects
      '("PROJECT/-MAYBE-DONE-ABORTED"
        ("NEXT")
        ("RECURRING")
        "\\<IGNORE\\>"))

(use-package ob
  :defer t
  :after org
  :config (progn
            (setq org-confirm-babel-evaluate nil
                  org-export-use-babel nil)
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((shell . t)
               (awk . t)
               (makefile . t)

               (ditaa . t)
               (plantuml . t)
               (dot . t)
               (gnuplot . t)
               (octave . t)

               (sqlite . t)

               (haskell . t)

               (C . t)

               (python . t)
               (perl . t)

               (java . t)

               (js . t)))
            (setq org-babel-C-compiler "gcc -std=c99"
                  org-babel-C++-compiler "g++ -std=c++14"
                  org-babel-python-command "python3"
                  org-babel-perl-preface "use 5.010;")))

(use-package org-crypt
  :after org
  :config (progn
            (org-crypt-use-before-save-magic)
            (add-to-list 'org-tags-exclude-from-inheritance "crypt")
            (setq org-crypt-key "890029F6")))

(use-package org-habit
  :after org-agenda
  :config (setq org-habit-show-habits-only-for-today nil
                org-habit-show-all-today nil
                org-agenda-show-future-repeats 'next
                org-habit-show-habits nil))


(use-package org-duration
  :after org
  :config (setq org-duration-units `(("min" . 1)
                                     ("h" . 60)
                                     ("d" . ,(* 60 8))
                                     ("w" . ,(* 60 8 5))
                                     ("m" . ,(* 60 8 5 4))
                                     ("y" . ,(* 60 8 5 4 11)))))

(use-package steam
  :straight t
  :defer t
  :config (setq steam-username "vifon"))

(provide 'my-org)
