(use-package el-patch
  :ensure t
  :config (setq el-patch-use-aggressive-defvar t))

(el-patch-feature dired)
(with-eval-after-load 'dired
  (el-patch-defun dired-copy-filename-as-kill (&optional arg)
    "Copy names of marked (or next ARG) files into the kill ring.
The names are separated by a space.
With a zero prefix arg, use the absolute file name of each marked file.
With \\[universal-argument], use the file name relative to the Dired buffer's
`default-directory'.  (This still may contain slashes if in a subdirectory.)

If on a subdir headerline, use absolute subdirname instead;
prefix arg and marked files are ignored in this case.

You can then feed the file name(s) to other commands with \\[yank]."
    (interactive "P")
    (let ((string
           (or (dired-get-subdir)
               (mapconcat (el-patch-wrap 3
                            (if (equal arg '(16))
                                #'shell-quote-argument
                              #'identity))
                          (if arg
                              (cond ((zerop (prefix-numeric-value arg))
                                     (dired-get-marked-files))
                                    ((consp arg)
                                     (dired-get-marked-files t))
                                    (t
                                     (dired-get-marked-files
                                      'no-dir (prefix-numeric-value arg))))
                            (dired-get-marked-files 'no-dir))
                          " "))))
      (unless (string= string "")
        (if (eq last-command 'kill-region)
            (kill-append string nil)
          (kill-new string))
        (message "%s" string)))))

(el-patch-feature dired-subtree)
(with-eval-after-load 'dired-subtree
  (el-patch-defun dired-subtree--readin (dir-name)
    "Read in the directory.

Return a string suitable for insertion in `dired' buffer."
    (el-patch-wrap 2
      (let ((orig-buffer (current-buffer)))
        (with-temp-buffer
          (insert-directory dir-name
                            (el-patch-swap
                              dired-listing-switches
                              (buffer-local-value 'dired-actual-switches
                                                  orig-buffer))
                            nil t)
          (delete-char -1)
          (goto-char (point-min))
          (delete-region
           (progn (beginning-of-line) (point))
           (progn (forward-line
                   (if (save-excursion
                         (forward-line 1)
                         (end-of-line)
                         (looking-back "\\."))
                       3 1))
                  (point)))
          (insert "  ")
          (while (= (forward-line) 0)
            (insert "  "))
          (delete-char -2)
          (buffer-string))))))

(el-patch-feature org-clock)
(with-eval-after-load 'org-clock
  (el-patch-defun org-clock-resolve (clock &optional prompt-fn last-valid fail-quietly)
    "Resolve an open Org clock.
An open clock was found, with `dangling' possibly being non-nil.
If this function was invoked with a prefix argument, non-dangling
open clocks are ignored.  The given clock requires some sort of
user intervention to resolve it, either because a clock was left
dangling or due to an idle timeout.  The clock resolution can
either be:

  (a) deleted, the user doesn't care about the clock
  (b) restarted from the current time (if no other clock is open)
  (c) closed, giving the clock X minutes
  (d) closed and then restarted
  (e) resumed, as if the user had never left

The format of clock is (CONS MARKER START-TIME), where MARKER
identifies the buffer and position the clock is open at (and
thus, the heading it's under), and START-TIME is when the clock
was started."
    (cl-assert clock)
    (let* ((ch
	        (save-window-excursion
	          (save-excursion
	            (unless org-clock-resolving-clocks-due-to-idleness
		          (org-clock-jump-to-current-clock clock))
	            (unless org-clock-resolve-expert
		          (with-output-to-temp-buffer "*Org Clock*"
		            (princ (format-message "Select a Clock Resolution Command:

i/q      Ignore this question; the same as keeping all the idle time.

k/K      Keep X minutes of the idle time (default is all).  If this
         amount is less than the default, you will be clocked out
         that many minutes after the time that idling began, and then
         clocked back in at the present time.

g/G      Indicate that you \"got back\" X minutes ago.  This is quite
         different from `k': it clocks you out from the beginning of
         the idle period and clock you back in X minutes ago.

s/S      Subtract the idle time from the current clock.  This is the
         same as keeping 0 minutes.

C        Cancel the open timer altogether.  It will be as though you
         never clocked in.

j/J      Jump to the current clock, to make manual adjustments.

For all these options, using uppercase makes your final state
to be CLOCKED OUT."))))
	            (org-fit-window-to-buffer (get-buffer-window "*Org Clock*"))
	            (let (char-pressed)
		          (while (or (null char-pressed)
			                 (and (not (memq char-pressed
					                         '(?k ?K ?g ?G ?s ?S ?C
						                          ?j ?J ?i ?q)))
				                  (or (ding) t)))
		            (setq char-pressed
			              (read-char (concat (funcall prompt-fn clock)
					                         " [jkKgGSscCiq]? ")
				                     nil 45)))
		          (and (not (memq char-pressed '(?i ?q))) char-pressed)))))
	       (default
	         (floor (org-time-convert-to-integer (org-time-since last-valid))
		            60))
	       (keep
	        (and (memq ch '(?k ?K))
	             (read-number "Keep how many minutes? " default)))
	       (gotback
	        (and (memq ch '(?g ?G))
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
                       (string-to-number (calc-eval input)))))))
	       (subtractp (memq ch '(?s ?S)))
	       (barely-started-p (org-time-less-p
			                  (org-time-subtract last-valid (cdr clock))
			                  45))
	       (start-over (and subtractp barely-started-p)))
      (cond
       ((memq ch '(?j ?J))
        (if (eq ch ?J)
	        (org-clock-resolve-clock clock 'now nil t nil fail-quietly))
        (org-clock-jump-to-current-clock clock))
       ((or (null ch)
	        (not (memq ch '(?k ?K ?g ?G ?s ?S ?C))))
        (message ""))
       (t
        (org-clock-resolve-clock
         clock (cond
	            ((or (eq ch ?C)
		             ;; If the time on the clock was less than a minute before
		             ;; the user went away, and they've ask to subtract all the
		             ;; time...
		             start-over)
	             nil)
	            ((or subtractp
		             (and gotback (= gotback 0)))
	             last-valid)
	            ((or (and keep (= keep default))
		             (and gotback (= gotback default)))
	             'now)
	            (keep
	             (org-time-add last-valid (* 60 keep)))
	            (gotback
	             (org-time-since (* 60 gotback)))
	            (t
	             (error "Unexpected, please report this as a bug")))
         (and gotback last-valid)
         (memq ch '(?K ?G ?S))
         (and start-over
	          (not (memq ch '(?K ?G ?S ?C))))
         fail-quietly)))))

  (el-patch-defun org-clock-resolve-clock
    (clock resolve-to clock-out-time close restart fail-quietly)
    "Resolve CLOCK given the time RESOLVE-TO, and the present.
CLOCK is a cons cell of the form (MARKER START-TIME)."
    (let ((org-clock-resolving-clocks t)
	      ;; If the clocked entry contained only a clock and possibly
	      ;; the associated drawer, and we either cancel it or clock it
	      ;; out, `org-clock-out-remove-zero-time-clocks' may clear all
	      ;; contents, and leave point on the /next/ headline.  We store
	      ;; the current entry location to be able to get back here when
	      ;; we need to clock in again the previously clocked task.
	      (heading (org-with-point-at (car clock)
		             (org-back-to-heading t)
		             (point-marker))))
      (pcase resolve-to
        (`nil
         (org-clock-clock-cancel clock)
         (when (and restart (not org-clock-clocking-in))
	       (org-with-point-at heading (org-clock-in))))
        (`now
         (cond
	      (restart (error "RESTART is not valid here"))
	      ((or close org-clock-clocking-in)
	       (org-clock-clock-out clock fail-quietly))
	      ((org-is-active-clock clock) nil)
	      (t (org-clock-clock-in clock t))))
        (el-patch-remove
          ((pred (org-time-less-p nil))
           (error "RESOLVE-TO must refer to a time in the past")))
        (_
         (when restart (error "RESTART is not valid here"))
         (org-clock-clock-out clock fail-quietly (or clock-out-time resolve-to))
         (cond
	      (org-clock-clocking-in nil)
	      (close
	       (setq org-clock-leftover-time (and (null clock-out-time) resolve-to)))
	      (t
	       (org-with-point-at heading
	         (org-clock-in nil (and clock-out-time resolve-to))))))))))

(provide 'my-el-patch)
