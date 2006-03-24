bin_PROGRAMS = mcxx

mcxx_CFLAGS=-Wall -g

mcxx_SOURCES = \
  cxx-ast.c \
  cxx-ast.h \
  cxx-lexer.h \
  cxx-parser.h \
  cxx-parser.c \
  cxx-scanner.h \
  cxx-scanner.c \
  cxx-driver.c \
  cxx-driver.h 


EXTRA_DIST =
EXTRA_DIST += cxx03.y
EXTRA_DIST += cxx03.l

cxx-parser.c cxx-parser.h : $(srcdir)/cxx03.y
	bison --debug --defines --report=all --output=cxx-parser.c $(srcdir)/cxx03.y

cxx-scanner.c : $(srcdir)/cxx03.l $(srcdir)/cxx-ast.h cxx-parser.h
	flex -Pmcxx -d -i -ocxx-scanner.c -8 $(srcdir)/cxx03.l

CLEANFILES =
CLEANFILES += cxx-parser.c
CLEANFILES += cxx-parser.h
CLEANFILES += cxx-scanner.c
