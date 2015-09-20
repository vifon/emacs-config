(use-package org
  :config (progn
            (define-key org-mode-map (kbd "<C-tab>") nil)
            (define-key org-mode-map (kbd "C-c C-x h") 'helm-org-in-buffer-headings)
            (define-key org-mode-map (kbd "C-c C-1")
              (lambda ()
                (interactive)
                (org-time-stamp-inactive '(16))))))

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
                  org-stored-links))))

(defadvice org-insert-heading-respect-content
    (after org-insert-heading-respect-content-with-empty-lines activate)
  (save-excursion
    (when (org-previous-line-empty-p)
      (insert "\n"))))

(defun org-followup ()
  (interactive)
  (let ((link (org-store-link nil)))
    (org-insert-heading-respect-content)
    (end-of-line)
    (save-excursion
      (insert "\n")
      (indent-for-tab-command)
      (org-time-stamp-inactive (quote (16)))
      (insert "\n")
      (indent-for-tab-command)
      (insert "Follow-up of: " link))))

(use-package org-protocol)

(setq org-hide-leading-stars nil)
(setq org-special-ctrl-a/e t)
(setq org-use-speed-commands t)

(defun org-minor-modes (&optional arg)
  (interactive "P")
  (org-bullets-mode arg)
  (hl-line-mode arg))
(defun my-org-mode-hook ()
  (make-local-variable 'electric-pair-pairs)
  (add-to-list 'electric-pair-pairs '(?$ . ?$))
  (org-minor-modes))
(add-hook 'org-mode-hook 'my-org-mode-hook)
(add-hook 'org-mobile-before-process-capture-hook
          (defun my-org-mobile-cleanup ()
            (interactive)
            (delete-blank-lines)
            (indent-region (point-min)
                           (point-max))
            (delete-trailing-whitespace (point-min)
                                        (point-max))
            (replace-regexp "\n+\\(\\*+ \\)"
                            "\n\n\\1"
                            nil
                            (point-min)
                            (point-max))
            (goto-char (point-max))
            (insert "\n")))

(setq org-default-notes-file (concat org-directory "/gtd.org"))

(setq org-refile-targets '((org-agenda-files :tag . "PROJECT")
                           (org-agenda-files :tag . "CATEGORY")
                           (org-agenda-files :tag . "GROUP")
                           (org-agenda-files :level . 1)
                           (nil :tag . "PROJECT")
                           (nil :tag . "CATEGORY")
                           (nil :tag . "GROUP")
                           (nil :level . 1)))
(setq org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-timestamp-if-done t)
(setq org-icalendar-combined-agenda-file "~/org/org.ics"
      org-icalendar-use-deadline '(event-if-todo)
      org-icalendar-use-scheduled '(event-if-todo)
      org-export-with-tasks 'todo)
(setq org-use-tag-inheritance nil)
(setq org-export-with-toc nil)
(setq org-tags-exclude-from-inheritance '("PROJECT" "ATTACH"))
(setq org-todo-keyword-faces '(("NEXT" . "Tomato")))
(setq org-enforce-todo-dependencies t)
(setq org-clock-into-drawer t)
(setq org-cycle-open-archived-trees nil)
(setq org-archive-default-command 'org-toggle-archive-tag)
(setq org-log-into-drawer t)
(setq org-hierarchical-todo-statistics nil)
(setq org-link-abbrev-alist '(("att" . org-attach-expand-link)))

(ignore-errors
  (load "~/org/.agenda-files.el"))
(when (not org-agenda-files)
  (setq org-agenda-files '("~/org/gtd.org")))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c S") 'org-store-link)
(global-set-key (kbd "C-c C-x C-o") 'org-clock-out)
(global-set-key (kbd "C-c C-x C-j") 'org-clock-goto)
(global-set-key (kbd "C-c C-x C-q") 'org-clock-cancel)
(global-set-key [f7] '(lambda ()
                        (interactive)
                        (find-file org-default-notes-file)))
(global-set-key [f8] '(lambda ()
                        (interactive)
                        (find-file (concat (vc-root-dir) "todo.org"))))

(setq org-capture-templates
      '(("t" "todo" entry (file+headline "" "Tasks")
         "* TODO %?\n  %U\n  %a\n" :empty-lines 1)

        ("T" "sub-todo" entry (clock)
         "* TODO %?\n  %U\n  %a\n" :empty-lines 1)

        ("b" "issue" entry (file+headline
                            (concat (or (vc-root-dir)
                                        (projectile-project-root)
                                        default-directory) "todo.org") "Issues")
         "* TODO %?\n  %U\n  %a\n" :empty-lines 1)

        ("s" "sub-task" item (clock)
         "- [ ] %?\n")

        ("S" "sub-task + link" item (clock)
         "- [ ] %?\n  %a\n")

        ("n" "note" entry (file "notes.org")
         "* %? :NOTE:\n  %U\n  %a\n" :empty-lines 1)

        ("p" "project" entry (file+headline "projects.org" "Ongoing")
         "* %? [/] :PROJECT:\n  %U\n" :empty-lines 1)

        ("j" "journal" entry (file+datetree "journal.org.gpg")
         "* %?" :empty-lines 0)

        ("J" "journal + prompt" entry (file+datetree+prompt "journal.org.gpg")
         "* %?" :empty-lines 0)))

(setq org-stuck-projects
      '("PROJECT" ("TODO") ("IGNORE") nil))

(plist-put org-format-latex-options :scale 2.0)

(defhydra hydra-org-template (:color blue :hint nil)
  "
_c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
_l_atex   _E_xample   _p_erl          _i_ndex:
_a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
_s_rc     ^ ^         plant_u_ml      _H_TML:
_h_tml    ^ ^         _S_hell         _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("S" (progn
         (hot-expand "<s")
         (insert "sh :results raw drawer")
         (forward-line)))
  ("e" (progn
         (hot-expand "<s")
         (insert "emacs-lisp")
         (forward-line)))
  ("p" (progn
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("u" (progn
         (hot-expand "<s")
         (insert "plantuml :file CHANGE.png")
         (forward-line)))
  ("P" (progn
         (insert "#+HEADERS: :results output :exports both :shebang \"#!/usr/bin/env perl\"\n")
         (hot-expand "<s")
         (insert "perl")
         (forward-line)))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str)
  "Expand org template."
  (insert str)
  (org-try-structure-completion))

(define-key org-mode-map "<"
  (lambda () (interactive)
    (if (looking-back "^")
        (hydra-org-template/body)
        (self-insert-command 1))))


(use-package ox
  :defer t
  :config (progn
            (use-package ox-reveal
              :load-path "~/.emacs.d/modules/org-reveal")
            (use-package ox-ioslide)))
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/2.6.2/")

(use-package ob
  :defer t
  :config (progn
            (setq org-confirm-babel-evaluate nil)
            (org-babel-do-load-languages
             'org-babel-load-languages
             '((sh . t)
               (awk . t)
               (sed . t)
               (makefile . t)

               (ditaa . t)
               (plantuml . t)
               (dot . t)
               (gnuplot . t)
               (octave . t)

               (sqlite . t)

               (C . t)

               (python . t)
               (perl . t)

               (java . t)
               (scala . t)))
            (setq org-babel-C-compiler "gcc -std=c99"
                  org-babel-C++-compiler "g++ -std=c++14")))

(use-package ob-ditaa
  :defer t
  :config (unless (file-exists-p org-ditaa-jar-path)
            (setq org-ditaa-jar-path
                  (cl-find-if
                   #'file-exists-p
                   '("/usr/share/ditaa/ditaa.jar"
                     "/usr/share/java/ditaa/ditaa-0_9.jar")))))

(use-package ob-plantuml
  :defer t
  :config (unless (file-exists-p org-plantuml-jar-path)
            (setq org-plantuml-jar-path
                  (cl-find-if
                   #'file-exists-p
                   '("~/.bin/plantuml.jar")))))


;;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-12/msg00231.html
(org-add-link-type "thunderlink" 'org-thunderlink-open)
(defun org-thunderlink-open (path)
  "Opens a specified email in Thunderbird with the help of the add-on ThunderLink."
  (start-process "thunderlink" nil
                 "thunderbird"
                 "-thunderlink"
                 (concat "thunderlink:"
                         (substring-no-properties path))))

(use-package org-crypt
  :init (progn
          (org-crypt-use-before-save-magic)
          (setq org-tags-exclude-from-inheritance '("crypt"))
          (setq org-crypt-key "B247B8DE")
          (define-key org-mode-map (kbd "C-c C-x C-k")
            (lambda (arg)
              (interactive "P")
              (if arg
                  (org-decrypt-entries)
                  (org-decrypt-entry))))))

(provide 'my-org)
