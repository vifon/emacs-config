(setq-default mode-line-format
              '("%e"
                mode-line-front-space
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
                mode-line-misc-info
                mode-line-end-spaces))

(setq mode-line-misc-info
      (let ((new-value (delete '(global-mode-string
                                 ("" global-mode-string " "))
                               mode-line-misc-info)))
        ;; Some packages (eyebrowse!) expect it to be a non-empty list.
        (if new-value
            new-value
          (list ""))))

(defface mode-line-read-only-face
  '((t :inherit mode-line-face
       :foreground "#60a5ff"
       :box (:line-width 2 :color "#60a5ff")))
  nil)
(defface mode-line-modified-face
  '((t :inherit mode-line-face
       :foreground "#c82829"
       :box (:line-width 2 :color "#c82829")))
  nil)
(defface mode-line-filename-face
  '((((background dark))
     :inherit mode-line-face
     :foreground "#eab700"
     :weight bold)
    (((background light))
     :inherit mode-line-face
     :foreground "#a98400"
     :weight bold))
  nil)
(defface mode-line-vposition-face
  '((t :inherit mode-line-face
       :weight bold))
  nil)
(defface mode-line-position-face
  '((t :inherit mode-line-face))
  nil)
(defface mode-line-80col-face
  '((t :inherit mode-line-position-face
       :foreground "black" :background "#eab700"))
  nil)
(defface mode-line-narrow-face
  '((t :inherit mode-line-face
       :foreground "red"))
  nil)

(provide 'my-mode-line)
