el-patch-requires = $(shell sed -n < 00-el-patch.el "s/el-patch-feature /require '/p")

00-el-patch.elc: 00-el-patch.el
	emacs --batch \
		-l ~/.emacs.d/init.el \
		--eval "(progn $(el-patch-requires))" \
		--eval "(byte-compile-file \"$(shell pwd)/$<\"))"
