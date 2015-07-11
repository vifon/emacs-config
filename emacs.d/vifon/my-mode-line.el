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
                ;; mode-line-modified
                mode-line-remote mode-line-frame-identification
                ;; (:eval (cond ((or (equal major-mode 'dired-mode) (equal major-mode 'wdired-mode))
                ;;               (shorten-directory default-directory 16 1))
                ;;              (buffer-file-name
                ;;               (shorten-directory default-directory 16 0))))
                (:propertize "%b"
                             face mode-line-filename-face)
                ;; mode-line-buffer-identification
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
                ;; mode-line-position
                (global-mode-string
                 ("" global-mode-string))
                (vc-mode vc-mode)
                " "
                mode-line-modes
                (which-funcion-mode
                 ("" which-func-format "--"))
                "---"
                mode-line-misc-info
                "-%-"
                ))

(setq mode-line-misc-info (delete '(global-mode-string
                                    ("" global-mode-string " "))
                                  mode-line-misc-info))

;; Helper function
(defun shorten-directory (dir max-length skip)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (dotimes (i (count ?/ (buffer-name)))
      (setq path (cdr path)))
    (loop for i from 0 below skip do
          (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))
;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-vposition-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-80col-face)
(make-face 'mode-line-narrow-face)

;; (set-face-attribute 'mode-line nil
;;                     :foreground "gray60" :background "gray20"
;;                     :inverse-video nil
;;                     :box '(:line-width 2 :color "gray20" :style nil))
;; (set-face-attribute 'mode-line-inactive nil
;;                     :foreground "gray80" :background "gray40"
;;                     :inverse-video nil
;;                     :box '(:line-width 2 :color "gray40" :style nil))

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
