all: ~/.emacs.d

~/.emacs.d: emacs.d
	emacs -Q --batch --eval " \
	(progn (require 'dired-x) \
	       (dired-make-relative-symlink \"$<\" \"$@\"))"
