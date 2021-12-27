(define-skeleton makefile-skeleton
  "" ""
  "CCFLAGS=-Wall -Wextra\n"
  "LDFLAGS=\n"
  "LDLIBS=\n"
  "\n"
  ".PHONY: all\n"
  "all: " (skeleton-read "Output file: " "main") "\n")

(define-auto-insert "/Makefile\\'" #'makefile-skeleton)
