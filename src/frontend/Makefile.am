lib_LTLIBRARIES = libmcxx-process.la libmcxx.la

END=

EXTRA_DIST =
CLEANFILES =
BUILT_SOURCES = 

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

# Overall process library containing global data
libmcxx_process_la_CFLAGS=-DLIBPROCESS_DLL_EXPORT \
                          -I$(top_srcdir)/lib \
                          -I$(top_srcdir)/src/driver \
                          -I$(top_srcdir)/src/frontend \
                          -I$(top_srcdir)/src/mcxx_tl \
                          -I$(top_builddir)/lib \
                          -I$(top_builddir)/src/driver \
                          -I$(top_builddir)/src/frontend \
                          -I$(top_builddir)/src/mcxx_tl \
                          $(END)
libmcxx_process_la_LDFLAGS=-avoid-version $(no_undefined)
libmcxx_process_la_LIBADD=$(top_builddir)/lib/libmcxx-utils.la
libmcxx_process_la_SOURCES = \
                             cxx-process.c \
                             cxx-process.h \
                             $(END)

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
  cxx-type-trie.h \
  cxx-type-trie.c \
  cxx-typeutils.c \
  cxx-typeutils.h \
  cxx-type-decls.h \
  cxx-typeenviron-decls.h \
  cxx-typeenviron.h \
  cxx-typeenviron.c \
  cxx-typeunif.c \
  cxx-typeunif.h \
  cxx-typeunif-decls.h \
  cxx-typededuc.c \
  cxx-typededuc.h \
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
  cxx-dyninit.c \
  cxx-dyninit.h \
  cxx-tltype.h \
  cxx-tltype.c \
  cxx-scopelink.c \
  cxx-scopelink-decls.h \
  cxx-scopelink.h \
  cxx-exprtype.h \
  cxx-exprtype.c \
  cxx-koenig.h \
  cxx-koenig.c \
  cxx-gccsupport-decls.h \
  cxx-gccsupport.h \
  cxx-gccsupport.c \
  cxx-gccbuiltins.h \
  cxx-gccbuiltins.c \
  cxx-gccspubuiltins.h \
  cxx-gccspubuiltins.c \
  cxx-upc.c \
  cxx-upc.h \
  cxx-typeorder.h \
  cxx-typeorder.c \
  cxx-overload.c \
  cxx-overload.h \
  libmcxx-common.h \
  $(END)

libmcxx_la_LIBADD = \
					$(top_builddir)/lib/libmcxx-utils.la \
					$(top_builddir)/lib/libmcxx-extstruct.la \
                    libmcxx-process.la \
					$(END)

if WINDOWS_BUILD
dll_exports=-Wl,$(srcdir)/dll-exports.def
else
dll_exports=
endif

# This is for Win32
EXTRA_DIST += dll-exports.def

libmcxx_la_LDFLAGS = $(dll_exports) -avoid-version $(no_undefined)

libmcxx_la_CFLAGS = -DLIBMCXX_DLL_EXPORT \
                    -Wall \
					-Wshadow \
					-Wextra \
				    -I$(top_srcdir)/lib \
				    -I$(top_srcdir)/src/driver \
                    -fexceptions \
				    -DPKGDATADIR=\"$(pkgdatadir)\"

EXTRA_DIST += cxx-lexer.l

# AST node types
AST_NODE_TYPE_FILES =
AST_NODE_TYPE_FILES += cxx-asttype-all.def
AST_NODE_TYPE_FILES += cxx-asttype-base.def
AST_NODE_TYPE_FILES += cxx-asttype-gcc.def
AST_NODE_TYPE_FILES += cxx-asttype-pragma.def
AST_NODE_TYPE_FILES += cxx-asttype-superscalar.def
AST_NODE_TYPE_FILES += cxx-asttype-upc.def

EXTRA_DIST += $(AST_NODE_TYPE_FILES)

EXTRA_DIST += cxx-attrnames.def

EXTRA_DIST += c99.l
EXTRA_DIST += c99.y
EXTRA_DIST += c99.y.in
EXTRA_DIST += c99-scanner.c

EXTRA_DIST += cxx03.l
EXTRA_DIST += cxx03.y
EXTRA_DIST += cxx03.y.in
EXTRA_DIST += cxx-scanner.c

EXTRA_DIST += cxx-subparse.y
EXTRA_DIST += cxx-pragma.y
EXTRA_DIST += cxx-construct.y
EXTRA_DIST += cxx-superscalar.y
EXTRA_DIST += cxx-ambig-handler.y
EXTRA_DIST += cxx-placeholders.y
EXTRA_DIST += c99-upc.y

EXTRA_DIST += cxx-gccspubuiltins.def

# Built sources, they are built the first
# otherwise parallel compilation is likely to fail
BUILT_SOURCES += cxx-attrnames.h
BUILT_SOURCES += cxx-attrnames.c

BUILT_SOURCES += cxx-asttype.def
BUILT_SOURCES += cxx-asttype.c
BUILT_SOURCES += cxx-asttype.h

BUILT_SOURCES += c99.l
BUILT_SOURCES += cxx03.l

BUILT_SOURCES += c99.y
BUILT_SOURCES += cxx03.y

BUILT_SOURCES += cxx-parser.h
BUILT_SOURCES += c99-parser.h

# Manual targets
cxx-ast.h : cxx-asttype.h

TPP=$(top_builddir)/lib/tpp$(EXEEXT)

CLEANFILES += cxx-asttype.h
cxx-asttype.h : cxx-asttype.def
	echo "#ifndef CXX_ASTTYPE_H" > cxx-asttype.h
	echo "#define CXX_ASTTYPE_H" >> cxx-asttype.h
	echo "#include \"libmcxx-common.h\"" >> cxx-asttype.h
	echo "#include \"cxx-macros.h\"" >> cxx-asttype.h
	echo "MCXX_BEGIN_DECLS" >> cxx-asttype.h
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" >> cxx-asttype.h
	echo "enum node_type {" >> cxx-asttype.h
	echo "AST_INVALID_NODE = 0,"  >> cxx-asttype.h
	sed -e "/^[[:blank:]]*\(#.*\)\?$$/d" -e "s/$$/,/" < cxx-asttype.def >> cxx-asttype.h
	echo "AST_LAST_NODE" >> cxx-asttype.h
	echo "};" >> cxx-asttype.h
	echo "LIBMCXX_EXTERN char* ast_node_names[];" >> cxx-asttype.h
	echo "typedef enum node_type node_t;" >> cxx-asttype.h
	echo "MCXX_END_DECLS" >> cxx-asttype.h
	echo "#endif // CXX_ASTTYPE_H" >> cxx-asttype.h

CLEANFILES += cxx-asttype.c
cxx-asttype.c : cxx-asttype.def cxx-asttype.h
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" > cxx-asttype.c
	echo "#include \"cxx-asttype.h\"" >> cxx-asttype.c
	echo "char* ast_node_names[] = {" >> cxx-asttype.c
	echo "[AST_INVALID_NODE] = \"AST_INVALID_NODE\", " >> cxx-asttype.c
	sed -e "/^[[:blank:]]*\(#.*\)\?$$/d" -e "s/^.*$$/[&] = \"&\",/" < cxx-asttype.def >> cxx-asttype.c
	echo "[AST_LAST_NODE] = \"AST_LAST_NODE\", " >> cxx-asttype.c
	echo "};" >> cxx-asttype.c

CLEANFILES += cxx-asttype.def
cxx-asttype.def : $(addprefix $(srcdir)/, $(AST_NODE_TYPE_FILES))
	$(TPP) -o cxx-asttype.def -I $(srcdir) $(srcdir)/cxx-asttype-all.def

CLEANFILES += cxx-attrnames.h
cxx-attrnames.h : $(srcdir)/cxx-attrnames.def
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" > cxx-attrnames.h
	echo "#ifndef CXX_ATTRNAMES_H" > cxx-attrnames.h
	echo "#define CXX_ATTRNAMES_H" >> cxx-attrnames.h
	echo "#include \"libmcxx-common.h\"" >> cxx-attrnames.h
	echo "#include \"cxx-macros.h\"" >> cxx-attrnames.h
	echo "MCXX_BEGIN_DECLS" >> cxx-attrnames.h
	sed -e "s/^\([^[:blank:]]*\)[[:blank:]]*\([^[:blank:]]*\)/LIBMCXX_EXTERN char \1[];/" < $< >> cxx-attrnames.h
	echo "LIBMCXX_EXTERN void register_ast_extended_attributes(void);" >> cxx-attrnames.h
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
	echo "    extensible_schema_init(&ast_extensible_schema);" >> cxx-attrnames.c
	sed -e "s/^\([^[:blank:]]*\)[[:blank:]]*\([^[:blank:]]*\)/    extensible_schema_add_field(\&ast_extensible_schema, \1, sizeof(tl_type_t));/" < $< >> cxx-attrnames.c
	echo "}" >> cxx-attrnames.c

if FLEX_BUILDING
CLEANFILES += cxx-scanner.c
CLEANFILES += cxx03.l
endif

cxx-scanner.c : cxx03.l cxx-parser.h
if FLEX_BUILDING
	$(FLEX) -Pmcxx -d -ocxx-scanner.c -8 cxx03.l
else
	@echo "*** ERROR: file cxx03.l was modified but no suitable flex was found during configure ***"
	@exit 1
endif

cxx03.l : $(srcdir)/cxx-lexer.l 
	$(TPP) -o cxx03.l -D CPLUSPLUS $(srcdir)/cxx-lexer.l

if BISON_BUILDING
CLEANFILES += cxx-parser.c
CLEANFILES += cxx-parser.h
CLEANFILES += cxx-parser.output
CLEANFILES += cxx03.y
endif

cxx03.y : $(srcdir)/cxx03.y.in $(srcdir)/cxx-placeholders.y $(srcdir)/cxx-subparse.y $(srcdir)/cxx-pragma.y $(srcdir)/cxx-construct.y $(srcdir)/cxx-superscalar.y $(srcdir)/cxx-ambig-handler.y
	$(TPP) -o cxx03.y -D CPLUSPLUS -I$(srcdir) $(srcdir)/cxx03.y.in

cxx-parser.c : cxx03.y
if BISON_BUILDING
	$(BISON) -pmcxx --debug --defines --report=all --output=cxx-parser.c cxx03.y
else
	@echo "*** ERROR: file cxx03.y was modified but no suitable bison-rofi was found during configure ***"
	@exit 1
endif

# These are obtained when invoking bison, if we write a rule like 'a b : d' it
# breaks parallel compilations since bison gets invoked twice.
cxx-parser.h : cxx-parser.c
cxx-parser.output : cxx-parser.c

if FLEX_BUILDING
CLEANFILES += c99-scanner.c
CLEANFILES += c99.l
endif

c99-scanner.c : c99.l c99-parser.h
if FLEX_BUILDING
	$(FLEX) -Pmc99 -d -oc99-scanner.c -8 c99.l
else
	@echo "*** ERROR: file c99.l was modified but no suitable flex was found during configure ***"
	@exit 1
endif

c99.l : $(srcdir)/cxx-lexer.l
	$(TPP) -o c99.l -D C99 $(srcdir)/cxx-lexer.l

if BISON_BUILDING
CLEANFILES += c99-parser.c
CLEANFILES += c99-parser.h
CLEANFILES += c99-parser.output
CLEANFILES += c99.y
endif

c99.y : $(srcdir)/c99.y.in $(srcdir)/cxx-placeholders.y $(srcdir)/cxx-subparse.y $(srcdir)/cxx-pragma.y $(srcdir)/cxx-construct.y $(srcdir)/cxx-superscalar.y $(srcdir)/cxx-ambig-handler.y $(srcdir)/c99-upc.y
	$(TPP) -o c99.y -D C99 -I$(srcdir) $(srcdir)/c99.y.in

c99-parser.c : c99.y
if BISON_BUILDING
	$(BISON) -pmc99 --debug --defines --report=all --output=c99-parser.c c99.y
else
	@echo "*** ERROR: file c99.y was modified but no suitable bison-rofi was found during configure ***"
	@exit 1
endif

# These are obtained when invoking bison, if we write a rule like 'a b : d' it
# breaks parallel compilations since bison gets invoked twice.
c99-parser.h : c99-parser.c
c99-parser.output : c99-parser.c
