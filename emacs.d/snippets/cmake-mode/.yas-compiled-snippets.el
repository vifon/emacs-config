;;; Compiled snippets and support files for `cmake-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'cmake-mode
                     '(("configure" "configure_file(\n  ${1:\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}}/${2:config.h}.in\n  ${3:\\$\\{CMAKE_CURRENT_BINARY_DIR\\}}/${4:$2}\n  @ONLY\n  )" "configure_file(...)" nil nil nil "/home/vifon/.emacs.d/snippets/cmake-mode/configure" nil nil)))


;;; Do not edit! File generated at Tue Mar 19 01:02:14 2019
