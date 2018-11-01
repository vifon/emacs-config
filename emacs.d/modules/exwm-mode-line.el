;;; exwm-mode-line.el ---

;; Copyright (C) 2018   Wojciech Siewierski

;; Author:  Wojciech Siewierski
;; Created: 01 Nov 2018

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(define-minor-mode display-exwm-workspace-mode
  "Display the EXWM workspace number in the mode-line."
  :global t :group 'exwm
  (if (bound-and-true-p exwm-workspace-current-index)
      (let ((mode-line-form '(:eval (propertize
                                     (format "X:%d "
                                             exwm-workspace-current-index)
                                     'face 'bold))))
        (or global-mode-string (setq global-mode-string '("")))
        (if display-exwm-workspace-mode
            (add-to-list 'global-mode-string
                         mode-line-form)
          (setq global-mode-string
                (delete mode-line-form
                        global-mode-string))))
    (message "EXWM not running")))

(provide 'exwm-mode-line)
;;; exwm-mode-line.el ends here
