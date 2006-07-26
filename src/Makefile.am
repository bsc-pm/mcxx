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
  cxx-scope.c \
  cxx-scope.h \
  cxx-buildscope.c \
  cxx-buildscope.h \
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
  cxx-printscope.c \
  cxx-printscope.h \
  cxx-instantiation.h \
  cxx-instantiation.c \
  cxx-fileextensions.c \
  cxx-configfile.h \
  cxx-configfile.c \
  cxx-configoptions.c

# Currently these files are unused but waiting for their momentum
#  cxx-koenig.c \
#  cxx-koenig.h \
#  cxx-typecalc.c \
#  cxx-typecalc.h \
#  cxx-overload.c \
#  cxx-overload.h

mcxx_LDFLAGS=-L../lib -L../gc
mcxx_LDADD=../lib/libmcxx-utils.la ../gc/libgc.la ../lib/libmcxx-mcfg.la
mcxx_CFLAGS=-Wall -g -I$(srcdir)/../lib -I$(srcdir)/../gc/include -DPKGDATADIR=\"$(pkgdatadir)\"

EXTRA_DIST =
EXTRA_DIST += cxx03.y
EXTRA_DIST += cxx03.l
EXTRA_DIST += cxx-asttype.def
EXTRA_DIST += cxx-fileextensions.gperf
EXTRA_DIST += cxx-configoptions.gperf

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

cxx-fileextensions.c : cxx-fileextensions.gperf cxx-driver.h
	gperf --language=ANSI-C --hash-function-name=fileextensions_hash --struct-type \
      --lookup-function-name=fileextensions_lookup --output=cxx-fileextensions.c \
      $(srcdir)/cxx-fileextensions.gperf

cxx-configoptions.c : cxx-configoptions.gperf cxx-driver.h cxx-configfile.h
	gperf --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=cxx-configoptions.c \
      $(srcdir)/cxx-configoptions.gperf

CLEANFILES =
CLEANFILES += cxx-parser.c
CLEANFILES += cxx-parser.h
CLEANFILES += cxx-scanner.c
CLEANFILES += cxx-asttype.h
CLEANFILES += cxx-asttype.c
CLEANFILES += cxx-parser.output
CLEANFILES += cxx-fileextensions.c
CLEANFILES += cxx-configoptions.c
