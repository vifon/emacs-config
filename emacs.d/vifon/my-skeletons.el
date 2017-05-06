(add-to-list 'load-path "~/.emacs.d/skeletons")
(load "~/.emacs.d/skeletons/asm-skeleton.el")
(load "~/.emacs.d/skeletons/c++-skeleton.el")
(load "~/.emacs.d/skeletons/cmake-skeleton.el")
(load "~/.emacs.d/skeletons/docker-skeleton.el")
(load "~/.emacs.d/skeletons/generic.el")
(load "~/.emacs.d/skeletons/haskell-skeleton.el")
(load "~/.emacs.d/skeletons/java-skeleton.el")
(load "~/.emacs.d/skeletons/makefile-skeleton.el")
(load "~/.emacs.d/skeletons/markdown-skeleton.el")
(load "~/.emacs.d/skeletons/org-skeleton.el")
(load "~/.emacs.d/skeletons/other-skeletons.el")
(load "~/.emacs.d/skeletons/perl-skeleton.el")
(load "~/.emacs.d/skeletons/python-skeleton.el")
(load "~/.emacs.d/skeletons/rust-skeleton.el")
(load "~/.emacs.d/skeletons/tex-skeleton.el")
(load "~/.emacs.d/skeletons/web-skeleton.el")

(use-package skeletor
  :defer t
  :config (progn
            (setq skeletor-project-directory "~/new-projects/")
            (skeletor-define-template "c++-project"
              :title "C++ Project")
            (skeletor-define-template "interactive-c"
              :title "Interactive C Project")
            (skeletor-define-template "nacl-project"
              :title "PNaCl Project")
            (skeletor-define-template "scala-project"
              :title "Scala Project"
              :requires-executables '(("sbt" . "http://www.scala-sbt.org/download.html"))
              :after-creation
              (lambda (dir)
                (skeletor-async-shell-command "sbt gen-ensime")
                (find-file "src/main/scala/Main.scala")))))

(provide 'my-skeletons)
