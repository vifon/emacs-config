;;; Compiled snippets and support files for `text-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'text-mode
                     '(("wtfpl" "DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE \nVersion 2, December 2004 \n\n`(insert \"Copyright (C) ${1:\\`(format-time-string \\\"%Y\\\")\\`}\")\n` ${2:`my-name`}${2:$(make-string (- 40 (length yas-text)) 32)}\n\nEveryone is permitted to copy and distribute verbatim or modified \ncopies of this license document, and changing it is allowed as long \nas the name is changed. \n\nDO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE \nTERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION \n\n0. You just DO WHAT THE FUCK YOU WANT TO.\n" "(wtfpl license)" nil nil
                        ((yas/after-exit-snippet-hook
                          (list
                           (let
                               ((comment-style 'box))
                             (comment-region
                              (point-min)
                              (point-max)
                              nil))))
                         (my-name "Wojciech Siewierski"))
                        "/home/vifon/.emacs.d/snippets/text-mode/wtfpl" nil nil)
                       ("time" "`(current-time-string)`" "(current time)" nil nil nil "/home/vifon/.emacs.d/snippets/text-mode/time" nil nil)
                       ("gpl" "`(insert \"Copyright (C) ${1:\\`(format-time-string \\\"%Y\\\")\\`}\")\n`  ${2:`my-name`}${2:$(make-string (- 40 (length yas-text)) 32)}\n\nThis program is free software: you can redistribute it and/or modify\nit under the terms of the GNU General Public License as published by\nthe Free Software Foundation, either version 3 of the License, or\n(at your option) any later version.\n\nThis program is distributed in the hope that it will be useful,\nbut WITHOUT ANY WARRANTY; without even the implied warranty of\nMERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\nGNU General Public License for more details.\n\nYou should have received a copy of the GNU General Public License\nalong with this program.  If not, see <http://www.gnu.org/licenses/>.\n" "(gnu gpl license)" nil nil
                        ((yas/after-exit-snippet-hook
                          (list
                           (let
                               ((comment-style 'box))
                             (comment-region
                              (point-min)
                              (point-max)
                              nil))))
                         (my-name "Wojciech Siewierski"))
                        "/home/vifon/.emacs.d/snippets/text-mode/gpl" nil nil)
                       ("email" "`(replace-regexp-in-string \"@\" \"@NOSPAM.\" user-mail-address)`" "(user's email)" nil nil nil "/home/vifon/.emacs.d/snippets/text-mode/email" nil nil)
                       ("=" "${1:2+2} = ${1:$(calc-eval\n   (replace-regexp-in-string \"p\" \"perm\"\n                 (replace-regexp-in-string \"c\" \"choose\"\n                                   yas-text)))}$0" "=" nil nil
                        ((yas-indent-line 'fixed))
                        "/home/vifon/.emacs.d/snippets/text-mode/=" nil nil)))


;;; Do not edit! File generated at Tue Mar 19 01:02:14 2019
