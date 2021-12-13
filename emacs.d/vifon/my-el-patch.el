;;; -*- lexical-binding: t; -*-

(use-package el-patch
  :straight t
  :config (setq el-patch-use-aggressive-defvar t))

(el-patch-feature dired)
(with-eval-after-load 'dired
  (el-patch-define-and-eval-template
   (defun dired-copy-filename-as-kill)
   (el-patch-wrap 3
     (if (equal arg '(16))
         #'shell-quote-argument
       #'identity))))

(el-patch-feature dired-subtree)
(with-eval-after-load 'dired-subtree
  (el-patch-define-and-eval-template
   (defun dired-subtree--readin)
   (el-patch-wrap 2
     (let ((orig-buffer (current-buffer)))
       (with-temp-buffer
         (insert-directory ...
                           (el-patch-swap
                             dired-listing-switches
                             (buffer-local-value 'dired-actual-switches
                                                 orig-buffer))
                           ...)
         ...)))))

(el-patch-feature org-clock)
(with-eval-after-load 'org-clock
  (el-patch-define-and-eval-template
   (defun org-clock-resolve)
   (el-patch-swap
     (read-number "Got back how many minutes ago? " default)
     (let ((input (read-string "Got back when? "
                               nil nil
                               (format-time-string "%H:%M" nil))))
       (if (string-match-p ":" input)
           (let* ((time (parse-time-string input))
                  (now (current-time))
                  (date (encode-time (cl-mapcar (lambda (a b)
                                                  (or a b))
                                                time
                                                (decode-time now)))))
             (floor (time-to-seconds (time-subtract now date))
                    60))
         (string-to-number (calc-eval input))))))

  (el-patch-define-and-eval-template
   (defun org-clock-resolve-clock)
   (el-patch-remove
     ((pred (time-less-p nil))
      (error ...)))))

(el-patch-feature org-capture)
(with-eval-after-load 'org-capture
  (el-patch-define-and-eval-template
   (defun org-capture-place-item)
   ((org-capture-get :target-entry-p)
    (cons (el-patch-wrap 2
            (or (re-search-forward org-logbook-drawer-re nil t)
                (line-beginning-position 2)))
          (org-entry-end-position)))
   (...
    (unless (and item prepend?) ...)
    (el-patch-add
      (unless item
        (indent-relative t t)))
    (org-capture-position-for-last-stored (point))
    ...)))
