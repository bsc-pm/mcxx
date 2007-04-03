SUBDIRS = tl .

bin_PROGRAMS = mcxx 

lib_LTLIBRARIES = libmcxx.la libmcxxtl.la

EXTRA_DIST =
CLEANFILES =
BUILT_SOURCES = 

# Gluing library C to C++
libmcxxtl_la_SOURCES = \
	cxx-compilerphases.hpp \
	cxx-compilerphases.cpp

libmcxxtl_la_CXXFLAGS=-I tl -I $(srcdir)/tl -I ../lib -I $(srcdir)/../lib -Wall

# Mercurium C/C++ compiler runtime library
libmcxx_la_SOURCES = \
  cxx-macros.h \
  cxx-asttype.h \
  cxx-asttype.c \
  cxx-ast.c \
  cxx-ast.h \
  cxx-ast-decls.h \
  cxx-lexer.h \
  cxx-parser.h \
  cxx-parser.c \
  cxx-scanner.c \
  c99-scanner.c \
  c99-parser.h \
  c99-parser.c \
  cxx-graphviz.h \
  cxx-graphviz.c \
  cxx-prettyprint.c \
  cxx-prettyprint.h \
  cxx-utils.c \
  cxx-utils.h \
  cxx-scope.c \
  cxx-scope.h \
  cxx-scope-decls.h \
  cxx-attrnames.h \
  cxx-attrnames.c \
  cxx-buildscope-decls.h \
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
  cxx-configoptions.c \
  cxx-debugflags.c \
  cxx-dyninit.c \
  cxx-dyninit.h \
  cxx-tltype.h \
  cxx-tltype.c \
  cxx-scopelink.c \
  cxx-scopelink-decls.h \
  cxx-scopelink.h

libmcxx_la_CFLAGS=-fexceptions -I$(srcdir)/../lib -DPKGDATADIR=\"$(pkgdatadir)\" -Wall

EXTRA_DIST += cxx-lexer.l
EXTRA_DIST += cxx-asttype.def
EXTRA_DIST += cxx-fileextensions.gperf
EXTRA_DIST += cxx-configoptions.gperf
EXTRA_DIST += cxx-debugflags.gperf
EXTRA_DIST += cxx-attrnames.def
EXTRA_DIST += c99.y.in
EXTRA_DIST += cxx03.y.in
EXTRA_DIST += cxx-omp.y
EXTRA_DIST += cxx-pragma.y
EXTRA_DIST += cxx-construct.y

BUILT_SOURCES += cxx-attrnames.h
BUILT_SOURCES += cxx-asttype.h

mcxx_SOURCES = \
  cxx-driver.c \
  cxx-driver-decls.h \
  cxx-driver.h

mcxx_LDFLAGS = \
   -L../lib \
   -L. \
   -Wl,-E

mcxx_LDADD = \
   -ldl \
   tl/libtl.la \
   ./libmcxx.la \
   ./libmcxxtl.la \
   ../lib/libmcxx-utils.la \
   ../lib/libmcxx-extstruct.la \
   ../lib/libmcxx-mcfg.la

mcxx_DEPENDENCIES =
mcxx_DEPENDENCIES += libmcxx.la
mcxx_DEPENDENCIES += libmcxxtl.la
mcxx_DEPENDENCIES += ./tl/libtl.la
mcxx_DEPENDENCIES += ../lib/libmcxx-utils.la
mcxx_DEPENDENCIES += ../lib/libmcxx-mcfg.la
mcxx_DEPENDENCIES += ../lib/libmcxx-extstruct.la

mcxx_CFLAGS=-fexceptions -I$(srcdir)/../lib -DPKGDATADIR=\"$(pkgdatadir)\" -Wall

# Mercurium C Compiler
# Is just mcxx copied to mcc

# Manual targets
cxx-ast.h : cxx-asttype.h

CLEANFILES += cxx-asttype.h
cxx-asttype.h : $(srcdir)/cxx-asttype.def
	echo "#ifndef CXX_ASTTYPE_H" > cxx-asttype.h
	echo "#define CXX_ASTTYPE_H" >> cxx-asttype.h
	echo "#include \"cxx-macros.h\"" >> cxx-asttype.h
	echo "MCXX_BEGIN_DECLS" >> cxx-asttype.h
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" >> cxx-asttype.h
	echo "enum node_type {" >> cxx-asttype.h
	echo "AST_INVALID_NODE = 0,"  >> cxx-asttype.h
	sed -e "s/$$/,/" < $(srcdir)/cxx-asttype.def >> cxx-asttype.h
	echo "AST_LAST_NODE" >> cxx-asttype.h
	echo "};" >> cxx-asttype.h
	echo "extern char* ast_node_names[];" >> cxx-asttype.h
	echo "typedef enum node_type node_t;" >> cxx-asttype.h
	echo "MCXX_END_DECLS" >> cxx-asttype.h
	echo "#endif // CXX_ASTTYPE_H" >> cxx-asttype.h

CLEANFILES += cxx-asttype.c
cxx-asttype.c : $(srcdir)/cxx-asttype.def cxx-asttype.h
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" > cxx-asttype.c
	echo "#include \"cxx-asttype.h\"" >> cxx-asttype.c
	echo "char* ast_node_names[] = {" >> cxx-asttype.c
	echo "[AST_INVALID_NODE] = \"AST_INVALID_NODE\", " >> cxx-asttype.c
	sed -e "s/^.*$$/[&] = \"&\",/" < $(srcdir)/cxx-asttype.def >> cxx-asttype.c
	echo "[AST_LAST_NODE] = \"AST_LAST_NODE\", " >> cxx-asttype.c
	echo "};" >> cxx-asttype.c

CLEANFILES += cxx-attrnames.h
cxx-attrnames.h : $(srcdir)/cxx-attrnames.def
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" > cxx-attrnames.h
	echo "#ifndef CXX_ATTRNAMES_H" > cxx-attrnames.h
	echo "#define CXX_ATTRNAMES_H" >> cxx-attrnames.h
	echo "#include \"cxx-macros.h\"" >> cxx-attrnames.h
	echo "MCXX_BEGIN_DECLS" >> cxx-attrnames.h
	sed -e "s/^\([^[:blank:]]*\)[[:blank:]]*\([^[:blank:]]*\)/extern char \1[];/" < $< >> cxx-attrnames.h
	echo "void register_ast_extended_attributes(void);" >> cxx-attrnames.h
	echo "MCXX_END_DECLS" >> cxx-attrnames.h
	echo "#endif // CXX_ATTRNAMES_H" >> cxx-attrnames.h

CLEANFILES += cxx-attrnames.c
cxx-attrnames.c : $(srcdir)/cxx-attrnames.def cxx-attrnames.h
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" > cxx-attrnames.c
	echo "#include \"cxx-attrnames.h\"" >> cxx-attrnames.c
	echo "#include \"cxx-ast.h\"" >> cxx-attrnames.c
	echo "#include \"cxx-tltype.h\"" >> cxx-attrnames.c
	sed -e "s/^\([^[:blank:]]*\)[[:blank:]]*\([^[:blank:]]*\)/char \1[] = \"\2\";/" < $< >> cxx-attrnames.c
	echo "/* AST attributes */" >> cxx-attrnames.c
	echo "void register_ast_extended_attributes(void)" >> cxx-attrnames.c
	echo "{" >> cxx-attrnames.c
	sed -e "s/^\([^[:blank:]]*\)[[:blank:]]*\([^[:blank:]]*\)/    extensible_schema_add_field(\&ast_extensible_schema, \1, sizeof(tl_type_t));/" < $< >> cxx-attrnames.c
	echo "}" >> cxx-attrnames.c

if GPERF_BUILDING
CLEANFILES += cxx-fileextensions.c
cxx-fileextensions.c : $(srcdir)/cxx-fileextensions.gperf $(srcdir)/cxx-driver.h
	gperf --language=ANSI-C --hash-function-name=fileextensions_hash --struct-type \
      --lookup-function-name=fileextensions_lookup --output=cxx-fileextensions.c \
      $(srcdir)/cxx-fileextensions.gperf

CLEANFILES += cxx-configoptions.c
cxx-configoptions.c : $(srcdir)/cxx-configoptions.gperf $(srcdir)/cxx-driver.h $(srcdir)/cxx-configfile.h
	gperf --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=cxx-configoptions.c \
      $(srcdir)/cxx-configoptions.gperf

CLEANFILES += cxx-debugflags.c
cxx-debugflags.c : $(srcdir)/cxx-debugflags.gperf $(srcdir)/cxx-driver.h $(srcdir)/cxx-utils.h
	gperf --language=ANSI-C --hash-function-name=debugflags_hash --struct-type \
      --lookup-function-name=debugflags_lookup --output=cxx-debugflags.c \
      --global-table --word-array-name=debugflags_list\
	  $(srcdir)/cxx-debugflags.gperf
endif

if FLEX_BUILDING
CLEANFILES += cxx-scanner.c
cxx-scanner.c : cxx03.l $(srcdir)/cxx-ast.h cxx-parser.h
	flex -Pmcxx -d -ocxx-scanner.c -8 cxx03.l

CLEANFILES += cxx03.l
cxx03.l : ../lib/tpp $(srcdir)/cxx-lexer.l
	../lib/tpp -o cxx03.l -D CPLUSPLUS $(srcdir)/cxx-lexer.l
endif

if BISON_BUILDING
CLEANFILES += cxx-parser.c
CLEANFILES += cxx-parser.h
CLEANFILES += cxx-parser.output
cxx-parser.c cxx-parser.h cxx-parser.output : cxx03.y
	bison -pmcxx --debug --defines --report=all --output=cxx-parser.c cxx03.y

CLEANFILES += cxx03.y
cxx03.y : $(srcdir)/cxx03.y.in $(srcdir)/cxx-omp.y $(srcdir)/cxx-pragma.y $(srcdir)/cxx-construct.y
	../lib/tpp -o cxx03.y -D CPLUSPLUS -I$(srcdir) $(srcdir)/cxx03.y.in
endif

if FLEX_BUILDING
CLEANFILES += c99-scanner.c
c99-scanner.c : c99.l $(srcdir)/cxx-ast.h c99-parser.h
	flex -Pmc99 -d -oc99-scanner.c -8 c99.l

CLEANFILES += c99.l
c99.l : $(srcdir)/cxx-lexer.l
	../lib/tpp -o c99.l -D C99 $(srcdir)/cxx-lexer.l
endif

if BISON_BUILDING
CLEANFILES += c99-parser.c
CLEANFILES += c99-parser.h
CLEANFILES += c99-parser.output
c99-parser.c c99-parser.h c99-parser.output : c99.y
	bison -pmc99 --debug --defines --report=all --output=c99-parser.c c99.y

CLEANFILES += c99.y
c99.y : $(srcdir)/c99.y.in $(srcdir)/cxx-omp.y $(srcdir)/cxx-pragma.y $(srcdir)/cxx-construct.y
	../lib/tpp -o c99.y -D C99 -I$(srcdir) $(srcdir)/c99.y.in
endif


# Install hooks
# installs "mcc" as a copy of mcxx
.PHONY : mcc
mcc :
install-exec-local:
	$(LIBTOOL) --mode=install --mode=install /usr/bin/install -c mcxx $(DESTDIR)/$(bindir)/mcc
