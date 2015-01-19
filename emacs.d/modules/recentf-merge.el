;;; Source: http://www.emacswiki.org/emacs/RecentFiles

(defun try-require (feature)
  (condition-case nil
      (require feature)
    (error (progn
             (message "could not require %s" feature)
             nil))))

(when (try-require 'recentf)
  (setq recentf-exclude '("~$"))
  (setq recentf-max-saved-items 99)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Here I override some functions of recentf to get a merged list
  ;; This seems to be stable (used it for approx two weeks at the time
  ;; of this writing)
  (defun recentf-save-list ()
    "Save the recent list.
Load the list from the file specified by `recentf-save-file',
merge the changes of your current session, and save it back to the
file."
    (interactive)
    (let ((instance-list (copy-list recentf-list)))
      (recentf-load-list)
      (recentf-merge-with-default-list instance-list)
      (recentf-write-list-to-file)))

  (defun recentf-merge-with-default-list (other-list)
    "Add all items from `other-list' to `recentf-list'."
    (dolist (oitem other-list)
      ;; add-to-list already checks for equal'ity
      (add-to-list 'recentf-list oitem)))

  (defun recentf-write-list-to-file ()
    "Write the recent files list to file.
Uses `recentf-list' as the list and `recentf-save-file' as the
file to write to."
    (condition-case error
        (with-temp-buffer
          (erase-buffer)
          (set-buffer-file-coding-system recentf-save-file-coding-system)
          (insert (format recentf-save-file-header (current-time-string)))
          (recentf-dump-variable 'recentf-list recentf-max-saved-items)
          (recentf-dump-variable 'recentf-filter-changer-current)
          (insert "\n \n;;; Local Variables:\n"
                  (format ";;; coding: %s\n" recentf-save-file-coding-system)
                  ";;; End:\n")
          (write-file (expand-file-name recentf-save-file))
          (when recentf-save-file-modes
            (set-file-modes recentf-save-file recentf-save-file-modes))
          nil)
      (error
       (warn "recentf mode: %s" (error-message-string error)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (recentf-mode 1))

(provide 'recentf-merge)
