(use-package org
  :ensure org-plus-contrib
  :pin org
  :config (progn
            (define-key org-mode-map (kbd "<C-tab>") nil)
            (define-key org-mode-map (kbd "C-c C-1")
              (lambda ()
                (interactive)
                (org-time-stamp-inactive '(16))))
            (setf (cdr (assoc "\\.pdf\\'"
                              org-file-apps))
                  "evince %s")
            (add-to-list 'org-file-apps '(directory . emacs))
            (dolist (ext '("png" "jpg" "jpeg"))
              (add-to-list 'org-file-apps (cons (concat "\\." ext "\\'")
                                                "sxiv %s")))))

(use-package org-attach
  :commands (org-attach-expand-link org-attach-attach)
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

(defun vifon/org-insert-unique-links ()
  "A wrapper for `org-insert-all-links' that skips the links
already present in the buffer."
  (interactive)
  (setq org-stored-links
        (cl-nset-difference
         org-stored-links
         (org-element-map (org-element-parse-buffer) 'link
           (lambda (link)
             (let ((path (org-element-property :path link))
                   (type (org-element-property :type link)))
               (when (equal type "file")
                 path))))
         :test (lambda (a b)
                 (string= (expand-file-name
                           (string-remove-prefix "file:"
                                                 (car
                                                  (split-string
                                                   (substring-no-properties
                                                    (car a))
                                                   "::"))))
                          (expand-file-name b)))))
  (call-interactively #'org-insert-all-links))

(require 'ol-notmuch)
(require 'org-protocol)
(require 'org-inlinetask)

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
(setq org-hide-emphasis-markers nil)

(setq org-default-notes-file (concat org-directory "/gtd.org"))

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

(use-package org-mru-clock :ensure t :defer t)

(setq org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t)
(setq org-cycle-open-archived-trees nil)
(setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-location ".archive/%s_archive::")
(setq org-log-into-drawer "LOGBOOK-NOTES"
      org-clock-into-drawer "LOGBOOK")
(setq org-log-reschedule 'time
      org-log-redeadline 'time)
(setq org-catch-invisible-edits 'smart)
(setq org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t)
(setq org-id-link-to-org-use-id 'use-existing)

(setq org-image-actual-width nil)

(setq org-confirm-elisp-link-function #'y-or-n-p)

(when (file-exists-p "~/org/.agenda-files")
  (setq org-agenda-files "~/org/.agenda-files"))
(when (not org-agenda-files)
  (setq org-agenda-files '("~/org/gtd.org")))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c S") 'org-store-link)

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "" "Inbox")
         "* TODO %?\n  %U\n  %a\n")

        ("T" "sub-todo" entry (clock)
         "* TODO %?\n  %U\n  %a\n")

        ("f" "follow-up" entry (file+headline "" "Inbox")
         "* TODO %?\n  %U\n  Follow-up of: %a\n")

        ("b" "issue" entry (file+headline
                            (concat (or (vc-root-dir)
                                        (projectile-project-root)
                                        default-directory) "todo.org") "Issues")
         "* TODO %?\n  %U\n  %a\n")

        ("n" "note" entry (file "notes.org")
         "* %? :NOTE:\n  %U\n  %a\n")

        ("j" "journal" entry (file+olp+datetree "journal.org.gpg")
         "* %?"
         :time-prompt t)

        ("p" "purchase" entry (file "purchases.org")
         "* %?\n  %U\n")))

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
      '("PROJECT/-MAYBE-DONE"
        ("NEXT")
        ("RECURRING")
        "\\<IGNORE\\>"))

(plist-put org-format-latex-options :scale 2.0)

(use-package ob
  :defer t
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

(org-add-link-type "evince" 'org-evince-open)
(defun org-evince-open (link)
  (string-match "\\(.*?\\)\\(?:::\\([0-9]+\\)\\)?$" link)
  (let ((path (match-string 1 link))
        (page (and (match-beginning 2)
                   (match-string 2 link))))
    (if page
        (call-process "evince" nil 0 nil
                      path "-i" page)
      (call-process "evince" nil 0 nil
                    path))))

(use-package org-crypt
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
  :config (setq org-duration-units `(("min" . 1)
                                     ("h" . 60)
                                     ("d" . ,(* 60 8))
                                     ("w" . ,(* 60 8 5))
                                     ("m" . ,(* 60 8 5 4))
                                     ("y" . ,(* 60 8 5 4 11)))))

(use-package steam
  :ensure t
  :defer t
  :config (setq steam-username "vifon"))

(provide 'my-org)
