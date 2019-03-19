;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
                     '(("tmpfile" "${1:finish}() {\n    rm -f \"\\$${2:TMP}\"\n    exit\n}; trap $1 EXIT INT TERM\n$2=\"\\$(mktemp -t ${3:prefix}.XXXXXX --tmpdir=/tmp\\)\"" "tmpfile" nil nil nil "/home/vifon/.emacs.d/snippets/sh-mode/tmpfile" nil nil)
                       ("if" "if [ ${1:-n \"$VAR\"} ]; then\n    $0\nfi" "if" nil nil nil "/home/vifon/.emacs.d/snippets/sh-mode/if" nil nil)
                       ("help" "help() {\n    cat <<EOF\n$1 -- $2\n\nUsage:\n  \\$0 $3\nEOF\n}\n\nwhile [ -n \"\\$1\" ]; do\n    case \"\\$1\" in\n        --help|-h)\n            help\n            exit\n            ;;\n        --)\n            shift\n            ;&\n        *)\n            break\n            ;;\n    esac\n    shift\ndone\n" "help" nil nil nil "/home/vifon/.emacs.d/snippets/sh-mode/help" nil nil)
                       ("getopts" "while getopts \"$1\" ${2:ARG}; do\n    case \"$$2\" in\n${1:$(mapconcat #'identity\n             (remove-if #'not\n                        (mapcar (lambda (x)\n                                  (when (not (equal x ?:))\n                                    (concat (byte-to-string x)\n                                            \")\\n            ;;\")))\n                                yas-text))\n             \"\\n        \")}\n        ?)\n            ;;\n    esac\ndone\nshift $((OPTIND-1))\n" "getopts" nil nil nil "/home/vifon/.emacs.d/snippets/sh-mode/getopts" nil nil)))


;;; Do not edit! File generated at Tue Mar 19 01:02:14 2019
