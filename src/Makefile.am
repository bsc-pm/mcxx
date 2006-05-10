bin_PROGRAMS = mcxx

mcxx_SOURCES = \
  cxx-asttype.h \
  cxx-asttype.c \
  cxx-ast.c \
  cxx-ast.h \
  cxx-lexer.h \
  cxx-parser.h \
  cxx-parser.c \
  cxx-scanner.c \
  cxx-driver.c \
  cxx-driver.h \
  cxx-graphviz.h \
  cxx-graphviz.c \
  cxx-prettyprint.c \
  cxx-prettyprint.h \
  cxx-utils.c \
  cxx-utils.h \
  cxx-symtab.c \
  cxx-symtab.h \
  cxx-buildsymtab.c \
  cxx-buildsymtab.h \
  cxx-typeutils.c \
  cxx-typeutils.h \
  cxx-typeunif.c \
  cxx-typeunif.h \
  cxx-cexpr.c \
  cxx-cexpr.h \
  cxx-ambiguity.c \
  cxx-ambiguity.h \
  cxx-solvetemplate.h \
  cxx-solvetemplate.c \
  cxx-printsymtab.c \
  cxx-printsymtab.h

mcxx_LDFLAGS=-L../lib
mcxx_LDADD=-lmcxx-utils 
mcxx_CFLAGS=-Wall -g -I$(srcdir)/../lib

EXTRA_DIST =
EXTRA_DIST += cxx03.y
EXTRA_DIST += cxx03.l
EXTRA_DIST += cxx-asttype.def

cxx-ast.h : cxx-asttype.h

cxx-asttype.h : $(srcdir)/cxx-asttype.def
	echo "#ifndef CXX_ASTTYPE_H" > cxx-asttype.h
	echo "#define CXX_ASTTYPE_H" >> cxx-asttype.h
	echo "/* This file has been generated. Every time you change cxx-asttype.def it will be regenerated */" >> cxx-asttype.h
	echo "enum node_type {" >> cxx-asttype.h
	sed -e "s/$$/,/" < $(srcdir)/cxx-asttype.def >> cxx-asttype.h
	echo "AST_LAST_NODE" >> cxx-asttype.h
	echo "};" >> cxx-asttype.h
	echo "extern char* ast_node_names[];" >> cxx-asttype.h
	echo "typedef enum node_type node_t;" >> cxx-asttype.h
	echo "#endif // CXX_ASTTYPE_H" >> cxx-asttype.h

cxx-asttype.c : $(srcdir)/cxx-asttype.def cxx-asttype.h
	echo "/* This file has been generated. Every time you change cxx-asttype.def it will be regenerated */" > cxx-asttype.c
	echo "#include \"cxx-asttype.h\"" >> cxx-asttype.c
	echo "char* ast_node_names[] = {" >> cxx-asttype.c
	sed -e "s/^.*$$/[&] = \"&\",/" < $(srcdir)/cxx-asttype.def >> cxx-asttype.c
	echo "};" >> cxx-asttype.c

cxx-parser.c cxx-parser.h : $(srcdir)/cxx03.y
	bison --debug --defines --report=all --output=cxx-parser.c $(srcdir)/cxx03.y

cxx-scanner.c : $(srcdir)/cxx03.l $(srcdir)/cxx-ast.h cxx-parser.h
	flex -Pmcxx -d -ocxx-scanner.c -8 $(srcdir)/cxx03.l

CLEANFILES =
CLEANFILES += cxx-parser.c
CLEANFILES += cxx-parser.h
CLEANFILES += cxx-scanner.c
CLEANFILES += cxx-asttype.h
CLEANFILES += cxx-asttype.c
CLEANFILES += cxx-parser.output
