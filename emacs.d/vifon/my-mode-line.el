(setq-default mode-line-format
              '("%e"
                "-"
                mode-line-mule-info mode-line-client
                (:eval
                 (cond ((and (buffer-modified-p) buffer-read-only)
                        (propertize "RO" 'face 'mode-line-modified-face))
                       (buffer-read-only
                        (propertize "RO" 'face 'mode-line-read-only-face))
                       ((buffer-modified-p)
                        (propertize "**" 'face 'mode-line-modified-face))
                       (t "--")))
                mode-line-remote mode-line-frame-identification
                (:propertize mode-line-buffer-identification
                             face mode-line-filename-face)
                " "
                "("
                (:propertize "%3l:" face mode-line-vposition-face)
                (:eval (propertize "%2c" 'face
                                   (if (>= (current-column) 80)
                                       'mode-line-80col-face
                                       'mode-line-position-face)))
                ") "
                (:propertize "[%p/%3I]" face mode-line-position-face)
                (:propertize "%n " face mode-line-narrow-face)
                (global-mode-string
                 ("" global-mode-string))
                (vc-mode vc-mode)
                " "
                mode-line-modes
                (which-funcion-mode
                 ("" which-func-format "--"))
                "---"
                mode-line-misc-info
                "-%-"))

(defun vifon--truncate-org-mode-line ()
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

(setq org-clock-heading-function #'vifon--truncate-org-mode-line)

(setq mode-line-misc-info (delete '(global-mode-string
                                    ("" global-mode-string " "))
                                  mode-line-misc-info))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-vposition-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-narrow-face)

(set-face-attribute 'mode-line-read-only-face nil
                    :inherit 'mode-line-face
                    :foreground "#60a5ff"
                    :box '(:line-width 2 :color "#60a5ff"))
(set-face-attribute 'mode-line-modified-face nil
                    :inherit 'mode-line-face
                    :foreground "#c82829"
                    :box '(:line-width 2 :color "#c82829"))
(set-face-attribute 'mode-line-filename-face nil
                    :inherit 'mode-line-face
                    :foreground "#eab700"
                    :weight 'bold)
(set-face-attribute 'mode-line-vposition-face nil
                    :inherit 'mode-line-face
                    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
                    :inherit 'mode-line-face)
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")
(set-face-attribute 'mode-line-narrow-face nil
                    :inherit 'mode-line-face
                    :foreground "red")
(provide 'my-mode-line)
