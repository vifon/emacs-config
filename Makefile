LN=ln -sfrT

all: ~/.emacs.d ~/.abbrev_defs

~/.emacs.d: emacs.d
	$(LN) $< $@

~/.abbrev_defs: abbrev_defs
	$(LN) $< $@
