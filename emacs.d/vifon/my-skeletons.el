(dolist (skeleton-file (directory-files "~/.emacs.d/skeletons/" t ".elc?$"))
  (load skeleton-file))

(use-package skeletor
  :ensure t
  :defer t
  :config (progn
            (setq skeletor-project-directory "~/new/")
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
                (find-file "src/main/scala/Main.scala")))
            (skeletor-define-template "web-frontend-project"
              :title "Web Frontend Project"
              :requires-executables '(("npm" . "https://www.npmjs.com/")))))

(provide 'my-skeletons)
