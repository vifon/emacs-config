;;; -*- lexical-binding: t; -*-

(use-package notmuch
  :straight t
  :if (and (file-directory-p "~/Mail/.notmuch")
           (file-regular-p   "~/.emacs.d/secret/notmuch-fcc"))
  :bind (("<f7>" . notmuch)
         ([remap compose-mail] . notmuch-mua-new-mail))
  :custom-face (notmuch-search-unread-face ((t (:weight extra-bold))))
  :config (progn
            (setq notmuch-fcc-dirs
                  (let ((tags "-new -unread -inbox +sent"))
                    (mapcar
                     (lambda (rule)
                       (let ((account (car rule))
                             (folder  (cdr rule)))
                         (cons account (format "\"%s\" %s"
                                               folder tags))))
                     (with-temp-buffer
                       ;; Format:
                       ;;   (("me@example.com"   . "me@example.com/Sent")
                       ;;    ("alsome@gmail.com" . "alsome@gmail.com/Sent Mail"))
                       (insert-file-contents "~/.emacs.d/secret/notmuch-fcc")
                       (goto-char (point-min))
                       (read (current-buffer))))))
            (setq message-signature
                  (lambda ()
                    (when (eq this-command #'message-insert-signature)
                      (let* ((signature-override
                              (expand-file-name
                               (message-sendmail-envelope-from)
                               "~/.signature.d/"))
                             (signature-file
                              (if (file-readable-p signature-override)
                                  signature-override
                                "~/.signature")))
                        (when (file-readable-p signature-file)
                          (with-temp-buffer
                            (insert-file-contents signature-file)
                            (buffer-string)))))))

            (setq mail-envelope-from 'header
                  send-mail-function 'sendmail-send-it)
            (setq notmuch-always-prompt-for-sender t)

            (setq message-auto-save-directory nil
                  message-confirm-send t)

            (setq notmuch-wash-signature-lines-max 3)

            (when (file-executable-p "~/.bin/notmuch-sync")
              (defun vifon/notmuch-poll-and-refresh-this-buffer ()
                (interactive)
                (call-process
                 "notmuch-sync" nil 0 nil
                 (buffer-name (current-buffer))))
              (dolist (map (list notmuch-hello-mode-map
                                 notmuch-show-mode-map
                                 notmuch-search-mode-map))
                (define-key map (kbd "G")
                  #'vifon/notmuch-poll-and-refresh-this-buffer)))

            (dolist (map (list notmuch-hello-mode-map
                               notmuch-show-mode-map))
              (define-key map (kbd "<C-tab>") nil))

            (bind-key "A"
                      (lambda ()
                        (interactive)
                        (when (y-or-n-p "Archive all?")
                          (notmuch-search-tag-all '("-unread" "-inbox"))))
                      notmuch-search-mode-map)
            (bind-key "D"
                      (lambda ()
                        (interactive)
                        (when (y-or-n-p "Delete all?")
                          (notmuch-search-tag-all '("-unread" "-inbox" "+deleted"))))
                      notmuch-search-mode-map)


            (defun notmuch-clear-search-history ()
              (interactive)
              (when (y-or-n-p "Clear the notmuch search history? ")
                (setq notmuch-search-history nil)
                (notmuch-refresh-this-buffer)))
            (bind-key "d" #'notmuch-clear-search-history
                      notmuch-hello-mode-map)
            (bind-key "C-c C-o"
                      (lambda (arg)
                        (interactive "P")
                        (if arg
                            (progn
                              (require 'shr)
                              (shr-next-link))
                          (require 'ffap)
                          (ffap-next-url)))
                      notmuch-show-mode-map)

            (bind-key "O"
                      (lambda () (interactive)
                        (setq notmuch-search-oldest-first
                              (not notmuch-search-oldest-first))
                        (message "Notmuch search oldest first: %s"
                                 notmuch-search-oldest-first))
                      notmuch-hello-mode-map)

            (defun notmuch-fcc-replace ()
              (interactive)
              (message-remove-header "Fcc")
              (notmuch-fcc-header-setup))

            (setq mm-tmp-directory (file-name-as-directory
                                    (concat
                                     "/tmp/mml-" (user-login-name))))
            (make-directory mm-tmp-directory t)

            (setq mml-secure-openpgp-sign-with-sender t)

            (setq shr-use-colors nil)

            (add-hook 'notmuch-message-mode-hook (lambda () (corfu-mode 0)))

            (defun vifon/notmuch-send-as (address)
              "Set a sender to ADDRESS, distinct from the From field.

Useful when sending from an alias address."
              (interactive
               (list (read-from-minibuffer
                      "From: " nil nil nil nil
                      (nth 1 (mail-extract-address-components
		                      (message-fetch-field "from"))))))
              (setq-local mail-specify-envelope-from t
                          mail-envelope-from address)
              (setq-local header-line-format `("Sending as " ,address)))))
