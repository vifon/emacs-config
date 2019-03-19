;;; Compiled snippets and support files for `cc-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cc-mode
                     '(("trace" "fprintf(${1:stderr}, \"${2:\\\\033\\[41;30m}%s:%d: ${3:%s}${2:$(if (not (string= yas-text \"\")) \"\\\\\\\\033\\[0m\" \"\")}\\\\n\", __FILE__, __LINE__${3:$(if (string-match \"%\" yas-text) \", \" \"\\);\")\n}$4${3:$(if (string-match \"%\" yas-text) \"\\);\" \"\")}" "fprintf(fd, ...)" nil nil nil "/home/vifon/.emacs.d/snippets/cc-mode/trace" nil nil)
                       ("inc" "#include <$1>" "#include <...>" nil nil nil "/home/vifon/.emacs.d/snippets/cc-mode/inc.1" nil nil)
                       ("inc" "#include \"$1\"" "#include \"...\"" nil nil nil "/home/vifon/.emacs.d/snippets/cc-mode/inc" nil nil)))


;;; Do not edit! File generated at Tue Mar 19 01:02:14 2019
