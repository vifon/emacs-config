.PHONY: all
all: ~/.emacs.d emacs.d/lisp

~/.emacs.d: emacs.d
	emacs -Q --batch --eval " \
	(progn (require 'dired-x) \
	       (dired-make-relative-symlink \"$<\" \"$@\"))"

.PHONY: emacs.d/lisp
emacs.d/lisp:
	make -C $@
