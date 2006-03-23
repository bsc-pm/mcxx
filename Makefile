SOURCES=\
  cxx-parser.c \
  cxx-lexer.c \
  cxx-ast.c

CC=gcc
CFLAGS=-Wall -g 

BISON=${HOME}/Universitat/rofi-bison-install/bin/bison
FLEX=flex
LIBS=

OBJECTS=$(subst .c,.o,$(SOURCES))

all : cxx

cxx : $(OBJECTS)
	$(CC) -o $@ $+ $(LIBS)

cxx-parser.c : cxx03.y
	$(BISON) --report=all -d -o $@ $<

cxx-lexer.c : cxx03.l cxx-parser.h
	$(FLEX) -o$@ -8 $<

.PHONY: clean
clean:
	rm -f cxx-lexer.c cxx-parser.c cxx-parser.h cxx-parser.output
	rm -f *.o cxx
