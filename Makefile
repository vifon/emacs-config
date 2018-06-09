LN=ln -sfrT

all: ~/.emacs.d

~/.emacs.d: emacs.d
	$(LN) $< $@
