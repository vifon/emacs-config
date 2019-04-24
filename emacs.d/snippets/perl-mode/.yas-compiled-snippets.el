;;; Compiled snippets and support files for `perl-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'perl-mode
                     '(("subm" "sub ${1:method_name} {\n    my $self = shift;\n    $0\n}" "sub ... { my $self = shift; ... }" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/subm" nil nil)
                       ("open" "open(${1:my $f}, '${2:<}', \"$3\")" "open( ... )" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/open" nil nil)
                       ("new" "sub new {\n    my $class = shift;\n    my $self = { $0 };\n    bless $self, $class;\n}" "sub new { ... }" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/new" nil nil)
                       ("met" "sub ${1:method_name} {\n    my $self = shift;\n    $0\n}" "method" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/method" nil nil)
                       ("has" "has '${1:attr}' => (\n    is  => '${2:rw}',\n    isa => '${3:Int}',$0\n);" "has" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/has" nil nil)
                       ("class" "package ${1:ClassName};\n\nuse Moose$2;\nuse namespace::autoclean;\n\n$0\n\n${3:__PACKAGE__->meta->make_immutable};" "class" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/class" nil nil)
                       ("bp" "$DB::single = 1; 1;" "breakpoint" nil nil nil "/home/vifon/.emacs.d/snippets/perl-mode/breakpoint" nil nil)))


;;; Do not edit! File generated at Thu Apr 25 00:56:11 2019
