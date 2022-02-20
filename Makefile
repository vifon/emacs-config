.PHONY: all
all: ~/.emacs.d emacs.d/lisp

~/.emacs.d: emacs.d
	emacs -Q --batch \
		-l dired-x \
		--eval "(unless (file-directory-p \"$@\") (dired-make-relative-symlink \"$<\" \"$@\"))"

.PHONY: emacs.d/lisp
emacs.d/lisp:
	make -C $@
