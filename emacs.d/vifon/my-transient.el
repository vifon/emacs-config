(use-package transient
  :commands (vifon/dired-transient)
  :bind ("C-c t" . vifon-transient)
  :config
  (transient-define-prefix vifon-transient ()
    "My main personal transient dispatcher."
    [["Cleanup"
      ("wc" "Whitespace cleanup" whitespace-cleanup)
      ("aa" "Align" align)
      ("ar" "Align regexp" align-regexp)
      ("s" "Sort lines" sort-lines)]
     [:description "Modes"
                   ("v" "Visual line mode" visual-line-mode :transient t)
                   ("O" "Olivetti" olivetti-mode :transient t)
                   ("r" "Truncate lines" toggle-truncate-lines :transient t)
                   ("ln" "Display line numbers" display-line-numbers-best :transient t)
                   ("lh" "Highlight current line" hl-line-mode :transient t)
                   ("ww" "Whitespace mode" whitespace-mode :transient t)
                   ("gf" "Display fill indicator" display-fill-column-indicator-mode :transient t)
                   ("gi" "Indent guide mode" indent-guide-mode :transient t)
                   ("fl" "Follow mode" follow-mode :transient t)]
     ["Others"
      ("F" "fly*-modes…" vifon/fly*-transient)
      ("o" "compare" compare-windows :transient t)
      ("nm" "normal-mode" normal-mode)
      ("tc" "TRAMP shutdown" tramp-cleanup-all-buffers)]])

  (transient-define-prefix vifon/fly*-transient ()
    "Flycheck, flyspell and the related commands."
    ["fly*-modes"
     ("c" "Flycheck mode" flycheck-mode :if-derived prog-mode)
     ("sm" "Flyspell prog mode" flyspell-prog-mode :if-derived prog-mode)
     ("sm" "Flyspell mode" flyspell-mode :if-not-derived prog-mode)
     ("sb" "Flyspell buffer" flyspell-buffer)
     ("i" "Ispell change dictionary" ispell-change-dictionary)])

  (autoload 'gnus-dired-mode "gnus-dired" nil t)
  (transient-define-prefix vifon/dired-transient ()
    "dired commands and submodes"
    [["Modes"
      ("a" "async" dired-async-mode :transient t)
      ("c" "Collapse mode" dired-collapse-mode :transient t)
      ("m" "Attachment mode" gnus-dired-mode :transient t)
      ("v" "diff-hl" (lambda ()
                       (interactive)
                       (call-interactively #'diff-hl-dired-mode)
                       (revert-buffer))
       :transient t)]
     ["Search"
      ("ff" "find-dired" find-dired)
      ("fn" "find-name-dired" find-name-dired)
      ("fg" "find-grep-dired" find-grep-dired)]]))
