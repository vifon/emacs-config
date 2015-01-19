(define-skeleton makefile-skeleton
  "" ""
  (let ((sources (mapconcat 'identity (merge 'list (file-expand-wildcards "*.c") (file-expand-wildcards "*.cpp") 'string-lessp) " "))
        (headers (mapconcat 'identity (merge 'list (file-expand-wildcards "*.h") (file-expand-wildcards "*.hpp") 'string-lessp) " ")))
    (concat
     "O=2
STD=c++11
CFLAGS=-Wall -Wextra -Weffc++ -O$(O) -std=$(STD)
LFLAGS=

SOURCES="sources"
HEADERS="headers"
OUT=a.out

.PHONY: all
all: release

.PHONY: release
release: $(OUT)

.PHONY: debug
debug: O=0
debug: CFLAGS+=-g
debug: $(OUT)

$(OUT): $(SOURCES) $(HEADERS)
	$(CXX) $(CFLAGS) $(SOURCES) $(LFLAGS) -o $(OUT)

.PHONY: clean
clean:
	rm -fv $(OUT) *.o
")))

(define-auto-insert "/Makefile$" 'makefile-skeleton)
