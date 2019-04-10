##########################################################################
#  (C) Copyright 2006-2013 Barcelona Supercomputing Center               #
#                          Centro Nacional de Supercomputacion           #
#                                                                        #
#  This file is part of Mercurium C/C++ source-to-source compiler.       #
#                                                                        #
#  See AUTHORS file in the top level directory for information           #
#  regarding developers and contributors.                                #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 3 of the License, or (at your option) any later version.      #
#                                                                        #
#  Mercurium C/C++ source-to-source compiler is distributed in the hope  #
#  that it will be useful, but WITHOUT ANY WARRANTY; without even the    #
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       #
#  PURPOSE.  See the GNU Lesser General Public License for more          #
#  details.                                                              #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with Mercurium C/C++ source-to-source compiler; if      #
#  not, write to the Free Software Foundation, Inc., 675 Mass Ave,       #
#  Cambridge, MA 02139, USA.                                             #
##########################################################################

ACLOCAL_AMFLAGS=-I m4

SUBDIRS = . tests

DIST_SUBDIRS = tests doc

# We should use AM_DISTCHECK_CONFIGURE_FLAGS but it is only available in Automake 1.11
DISTCHECK_CONFIGURE_FLAGS =
# We need to augment the command line variables
override DISTCHECK_CONFIGURE_FLAGS += --enable-distcheck-processing=yes

# This rule generates DIST_REVISION
git-version-dist-hook:
	if test x"$(GIT)" != x; \
	then \
	    run_gitversion=`"$(GIT)" --git-dir=$(srcdir)/.git show "--pretty=format:%h %ci" HEAD | head -n 1`; \
	    build_version="$${run_gitversion}"; \
	    echo "$${build_version}" > $(distdir)/DIST_REVISION; \
	elif [ -e $(srcdir)/DIST_REVISION ]; \
	then \
	    cp $(srcdir)/DIST_REVISION $(distdir)/DIST_REVISION; \
	fi

##########################################################################
##  Directories                                                       ####
##########################################################################

# Make sure you use the full path (relative from this Makefile.am)
# Files will be built in the appropiate directory

# Global variables
#
# Any variable appearing here can only be += afterwards
# (otherwise you would be overwriting it!)
EXTRA_DIST =
END =
noinst_LIBRARIES=
noinst_LTLIBRARIES =
noinst_PROGRAMS =
lib_LTLIBRARIES =
CLEANFILES =

BUILT_SOURCES =

# These variables are not intended for any further modification
if WINDOWS_BUILD
no_undefined=-no-undefined
dll_exports=-Wl,src/frontend/dll-exports.def
else
no_undefined=
dll_exports=
endif

AM_CFLAGS=
AM_CPPFLAGS=
AM_CXXFLAGS=
AM_LDFLAGS=

if EXTRAE_ENABLED
AM_CPPFLAGS   += -I@EXTRAE_INCLUDE@
AM_LDFLAGS  += -L@EXTRAE_LIB@ -lseqtrace
endif


##########################################################################
# gnulib
##########################################################################
#
# This library was used when building Mercurium in Solaris
# In Linux it does nothing

EXTRA_DIST +=\
	 gnulib/tempname.c \
     gnulib/tempname.h \
     gnulib/mkdtemp.c \
     gnulib/siglist.h \
     gnulib/strsignal.c \
     $(END)

noinst_LTLIBRARIES += gnulib/libgnulib.la

gnulib_libgnulib_la_SOURCES = gnulib/dummy.c

gnulib_libgnulib_la_LIBADD=$(LTLIBOBJS)

##########################################################################
# lib
##########################################################################

noinst_PROGRAMS += lib/tpp

CC_FOR_BUILD=@CC_FOR_BUILD@

if SUPPORTED_SILENT_RULES
CCBUILD_verbose = $(CCBUILD_verbose_$(V))
CCBUILD_verbose_ = $(CCBUILD_verbose_$(AM_DEFAULT_VERBOSITY))
CCBUILD_verbose_0 = @echo "  CCBUILD " $@;
endif

# tpp
lib_tpp_SOURCES = lib/tpp.c

lib/tpp$(EXEEXT): lib/tpp.o
	$(CCBUILD_verbose)$(CC_FOR_BUILD) -o $@ $+ $(LDFLAGS)

lib/tpp.o : lib/tpp.c
	$(CCBUILD_verbose)$(CC_FOR_BUILD) $(CFLAGS) -c -o $@ $(top_srcdir)/lib/tpp.c $(DEFAULT_INCLUDES) \
	         -DPACKAGE=\"$(PACKAGE)\" -DVERSION=\"$(PACKAGE_VERSION)\"
#end of tpp


lib_LTLIBRARIES += lib/libmcxx-utils.la

lib_libmcxx_utils_la_CFLAGS = -std=gnu99 -Wall -DLIBUTILS_DLL_EXPORT
lib_libmcxx_utils_la_SOURCES = \
						  lib/mcxx_refcount.c \
						  lib/mcxx_refcount.h \
						  lib/uniquestr.h \
						  lib/char_hash.c \
                          lib/libutils-common.h \
                          lib/filename.c \
                          lib/filename.h \
                          lib/string_utils.h \
                          lib/string_utils.c \
                          lib/dhash_str.c \
                          lib/dhash_str.h \
                          lib/dhash_ptr.c \
                          lib/dhash_ptr.h \
                          lib/red_black_tree.c \
                          lib/red_black_tree.h \
                          lib/mem.c \
                          lib/mem.h \
                          $(END)

lib_libmcxx_utils_la_LDFLAGS= -avoid-version $(no_undefined)
lib_libmcxx_utils_la_LIBADD= -lm

BUILT_SOURCES += lib/perish.o
CLEANFILES += lib/perish.o

EXTRA_DIST += lib/perish.c
lib/perish.o : lib/perish.c
	$(CCBUILD_verbose)$(CC_FOR_BUILD) $(CFLAGS) -c -o $@ $(top_srcdir)/lib/perish.c $(DEFAULT_INCLUDES) \
	         -DPACKAGE=\"$(PACKAGE)\" -DVERSION=\"$(PACKAGE_VERSION)\"


##########################################################################
# src
##########################################################################

# Empty

##########################################################################
# src/frontend
##########################################################################

# These two variables are used again in src/tl
NODECL_GENERATOR=$(top_srcdir)/src/frontend/nodecl-generator.py
NODECL_DEF=$(top_srcdir)/src/frontend/cxx-nodecl.def


lib_LTLIBRARIES += \
				 src/frontend/libmcxx-process.la \
				 src/frontend/libmcxx.la

# Overall process library containing global data
src_frontend_libmcxx_process_la_CFLAGS=-std=gnu99 \
			  -DLIBMCXXPROCESS_DLL_EXPORT \
                          -I$(top_srcdir)/lib \
			  -I$(top_srcdir)/support/gperf \
			  -I$(top_builddir)/support/gperf \
                          -I$(top_srcdir)/src/driver \
                          -I$(top_srcdir)/src/frontend \
                          -I$(top_srcdir)/src/frontend/fortran \
                          -I$(top_srcdir)/src/mcxx_tl \
                          -I$(top_builddir)/lib \
                          -I$(top_builddir)/src/driver \
                          -I$(top_builddir)/src/frontend \
                          -I$(top_builddir)/src/frontend/fortran \
                          -I$(top_builddir)/src/mcxx_tl \
                          $(END)

src_frontend_libmcxx_process_la_LDFLAGS=-avoid-version $(no_undefined)
src_frontend_libmcxx_process_la_LIBADD=./lib/libmcxx-utils.la
src_frontend_libmcxx_process_la_SOURCES = \
                             src/frontend/cxx-process.c \
                             src/frontend/cxx-process.h \
                             $(END)

# Mercurium C/C++ compiler runtime library
src_frontend_libmcxx_la_SOURCES = \
  src/frontend/cxx-limits.h \
  src/frontend/cxx-macros.h \
  src/frontend/cxx-locus.h \
  src/frontend/cxx-locus-inline.h \
  src/frontend/cxx-locus.c \
  src/frontend/cxx-asttype.h \
  src/frontend/cxx-asttype.c \
  src/frontend/cxx-asttype-str.c \
  src/frontend/cxx-asttype-str.h \
  src/frontend/cxx-asttype-str-internal.h \
  src/frontend/cxx-ast.c \
  src/frontend/cxx-ast.h \
  src/frontend/cxx-ast-inline.h \
  src/frontend/cxx-ast-fwd.h \
  src/frontend/cxx-ast-decls.h \
  src/frontend/cxx-nodecl.h \
  src/frontend/cxx-nodecl-inline.h \
  src/frontend/cxx-nodecl.c \
  src/frontend/cxx-nodecl-fwd.h \
  src/frontend/cxx-nodecl-decls.h \
  src/frontend/cxx-nodecl-checker.h \
  src/frontend/cxx-nodecl-checker.c \
  src/frontend/cxx-nodecl-output.h \
  src/frontend/cxx-nodecl-output.c \
  src/frontend/cxx-nodecl-visitor.h \
  src/frontend/cxx-nodecl-visitor.c \
  src/frontend/cxx-nodecl-shallow-copy.c \
  src/frontend/cxx-nodecl-deep-copy-fwd.h \
  src/frontend/cxx-nodecl-deep-copy.h \
  src/frontend/cxx-nodecl-deep-copy-base.c \
  src/frontend/cxx-nodecl-deep-copy.c \
  src/frontend/cxx-symbol-deep-copy-entity-specs.c \
  src/frontend/cxx-symbol-deep-copy.h \
  src/frontend/cxx-symbol-deep-copy.c \
  src/frontend/cxx-lexer.h \
  src/frontend/cxx-parser.h \
  src/frontend/cxx-parser-internal.h \
  src/frontend/cxx-parser.c \
  src/frontend/cxx-scanner.c \
  src/frontend/c99-scanner.c \
  src/frontend/c99-parser.h \
  src/frontend/c99-parser-internal.h \
  src/frontend/c99-parser.c \
  src/frontend/cxx-graphviz.h \
  src/frontend/cxx-graphviz.c \
  src/frontend/cxx-html.h \
  src/frontend/cxx-html.c \
  src/frontend/cxx-prettyprint.c \
  src/frontend/cxx-prettyprint.h \
  src/frontend/cxx-prettyprint-internal.h \
  src/frontend/cxx-utils.c \
  src/frontend/cxx-utils.h \
  src/frontend/cxx-scope.c \
  src/frontend/cxx-scope.h \
  src/frontend/cxx-entrylist-decls.h \
  src/frontend/cxx-entrylist.c \
  src/frontend/cxx-entrylist.h \
  src/frontend/cxx-scope-fwd.h \
  src/frontend/cxx-scope-decls.h \
  src/frontend/cxx-buildscope-decls.h \
  src/frontend/cxx-buildscope.c \
  src/frontend/cxx-buildscope.h \
  src/frontend/cxx-type-trie.h \
  src/frontend/cxx-type-trie.c \
  src/frontend/cxx-typeutils.c \
  src/frontend/cxx-typeutils.h \
  src/frontend/cxx-type-fwd.h \
  src/frontend/cxx-type-decls.h \
  src/frontend/cxx-typeenviron-decls.h \
  src/frontend/cxx-typeenviron.h \
  src/frontend/cxx-typeenviron.c \
  src/frontend/cxx-typededuc.c \
  src/frontend/cxx-typededuc.h \
  src/frontend/cxx-typededuc-decls.h \
  src/frontend/cxx-cexpr.c \
  src/frontend/cxx-cexpr.h \
  src/frontend/cxx-cexpr-fwd.h \
  src/frontend/cxx-cexpr-decls.h \
  src/frontend/cxx-cexpr-deep-copy.h \
  src/frontend/cxx-cexpr-deep-copy.c \
  src/frontend/cxx-ambiguity.c \
  src/frontend/cxx-ambiguity.h \
  src/frontend/cxx-solvetemplate.h \
  src/frontend/cxx-solvetemplate.c \
  src/frontend/cxx-printscope.c \
  src/frontend/cxx-printscope.h \
  src/frontend/cxx-instantiation.h \
  src/frontend/cxx-instantiation-decls.h \
  src/frontend/cxx-instantiation.c \
  src/frontend/cxx-dyninit.c \
  src/frontend/cxx-dyninit.h \
  src/frontend/cxx-tltype.h \
  src/frontend/cxx-tltype.c \
  src/frontend/cxx-exprtype-decls.h \
  src/frontend/cxx-exprtype.h \
  src/frontend/cxx-exprtype.c \
  src/frontend/cxx-koenig.h \
  src/frontend/cxx-koenig.c \
  src/frontend/cxx-gccsupport-decls.h \
  src/frontend/cxx-gccsupport.h \
  src/frontend/cxx-gccsupport.c \
  src/frontend/cxx-mssupport.h \
  src/frontend/cxx-mssupport.c \
  src/frontend/cxx-pragma.h \
  src/frontend/cxx-pragma.c \
  src/frontend/cxx-upc.c \
  src/frontend/cxx-upc.h \
  src/frontend/cxx-cuda.c \
  src/frontend/cxx-cuda.h \
  src/frontend/cxx-typeorder.h \
  src/frontend/cxx-typeorder.c \
  src/frontend/cxx-overload.c \
  src/frontend/cxx-overload-decls.h \
  src/frontend/cxx-overload.h \
  src/frontend/cxx-codegen.h \
  src/frontend/cxx-diagnostic.c \
  src/frontend/cxx-diagnostic.h \
  src/frontend/cxx-placeholders.c \
  src/frontend/cxx-placeholders.h \
  src/frontend/libmcxx-common.h \
  $(END)

src_frontend_libmcxx_la_LIBADD = \
					./lib/libmcxx-utils.la \
                    ./src/frontend/libmcxx-process.la \
                    ./src/frontend/libgccbuiltins.la \
					$(quadmath_LIBS) \
					$(END)

# This is for Win32
EXTRA_DIST += src/frontend/dll-exports.def

src_frontend_libmcxx_la_LDFLAGS = $(dll_exports) -avoid-version $(no_undefined)

cfrontend_cflags = \
		   -std=gnu99 \
		   -DLIBMCXX_DLL_EXPORT \
		   -Wall \
		   -Wshadow \
		   -Wextra \
		   -I$(top_srcdir)/lib \
		   -I$(top_srcdir)/support/gperf \
		   -I$(top_builddir)/support/gperf \
		   -I$(top_srcdir)/src/driver \
		   -I$(top_srcdir)/src/mcxx_tl \
		   -I$(top_srcdir)/src/frontend \
		   -I$(top_srcdir)/src/frontend/fortran \
		   -I./src/frontend \
		   -I./src/frontend/fortran \
		   -fexceptions \
		   -DPKGDATADIR=\"$(pkgdatadir)\"

src_frontend_libmcxx_la_CFLAGS = $(cfrontend_cflags)

noinst_LTLIBRARIES += src/frontend/libgccbuiltins.la

src_frontend_libgccbuiltins_la_LDFLAGS=-avoid-version $(no_undefined)
src_frontend_libgccbuiltins_la_CFLAGS=@NO_VAR_TRACKING_ASSIGNMENTS@ \
									  $(cfrontend_cflags)

src_frontend_libgccbuiltins_la_SOURCES = \
  src/frontend/cxx-gccbuiltins.h \
  src/frontend/cxx-gccbuiltins-ia32.h \
  src/frontend/cxx-gccbuiltins-ia32-deprecated.h \
  src/frontend/cxx-gccbuiltins-arm-neon.h \
  src/frontend/cxx-gccbuiltins-arm64-neon.h \
  src/frontend/cxx-gccbuiltins.c \
  src/frontend/cxx-gccspubuiltins.h \
  src/frontend/cxx-gccspubuiltins.c \
  \
  src/frontend/cxx-iccbuiltins.h \
  src/frontend/cxx-iccbuiltins-knc.h \
  src/frontend/cxx-intelsupport.h \
  src/frontend/cxx-intelsupport.c \
  $(END)


EXTRA_DIST += src/frontend/cxx-lexer.l

# AST node types
AST_NODE_TYPE_FILES =
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-all.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-base.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-extensions.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-gcc.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-pragma.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-superscalar.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-upc.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-omp.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-ompss.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-cuda.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-opencl.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-xl.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-ms.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-intel.def
AST_NODE_TYPE_FILES += src/frontend/cxx-asttype-fortran.def
# Generated
AST_NODE_TYPE_FILES_GEN =
AST_NODE_TYPE_FILES_GEN += src/frontend/cxx-asttype-nodecl.def

EXTRA_DIST += $(AST_NODE_TYPE_FILES) $(AST_NODE_TYPE_FILES_GEN)

EXTRA_DIST += src/frontend/c99.l
EXTRA_DIST += src/frontend/c99.y
EXTRA_DIST += src/frontend/c99.y.in
EXTRA_DIST += src/frontend/c99-scanner.c

EXTRA_DIST += src/frontend/cxx03.l
EXTRA_DIST += src/frontend/cxx03.y
EXTRA_DIST += src/frontend/cxx03.y.in
EXTRA_DIST += src/frontend/cxx-scanner.c

EXTRA_DIST += src/frontend/cxx-asttype.def

ADDITIONAL_GRAMMARS =
ADDITIONAL_GRAMMARS += src/frontend/cxx-subparse.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-literal-nodes.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-pragma.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-omp.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-ompss.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-superscalar.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-ambig-handler.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-placeholders.y
ADDITIONAL_GRAMMARS += src/frontend/c99-upc.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-cuda.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-opencl.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-xl.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-ms.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-intel.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-extensions.y
ADDITIONAL_GRAMMARS += src/frontend/cxx-fortran-compat.y

EXTRA_DIST += $(ADDITIONAL_GRAMMARS)

EXTRA_DIST += src/frontend/cxx-gccspubuiltins.def

# Built sources, they are built the first
# otherwise parallel compilation is likely to fail

BUILT_SOURCES += src/frontend/cxx-asttype.c
BUILT_SOURCES += src/frontend/cxx-asttype.h

BUILT_SOURCES += src/frontend/c99.l
BUILT_SOURCES += src/frontend/cxx03.l

BUILT_SOURCES += src/frontend/c99.y
BUILT_SOURCES += src/frontend/cxx03.y

BUILT_SOURCES += src/frontend/cxx-parser.h
BUILT_SOURCES += src/frontend/c99-parser.h

EXTRA_DIST += src/frontend/gen-symbol-attrs.py
EXTRA_DIST += src/frontend/cxx-entity-specs.def

EXTRA_DIST += src/frontend/nodecl-generator.py
EXTRA_DIST += src/frontend/cxx-nodecl.def

EXTRA_DIST += src/frontend/cxx-asttype-str.gperf

TPP=./lib/tpp$(EXEEXT)

# Silent rules
if SUPPORTED_SILENT_RULES
BISON_verbose = $(BISON_verbose_$(V))
BISON_verbose_ = $(BISON_verbose_$(AM_DEFAULT_VERBOSITY))
BISON_verbose_0 = @echo "  BISON " $@;

FLEX_verbose = $(FLEX_verbose_$(V))
FLEX_verbose_ = $(FLEX_verbose_$(AM_DEFAULT_VERBOSITY))
FLEX_verbose_0 = @echo "  FLEX  " $@;

TPP_verbose = $(TPP_verbose_$(V))
TPP_verbose_ = $(TPP_verbose_$(AM_DEFAULT_VERBOSITY))
TPP_verbose_0 = @echo "  TPP   " $@;

PYTHON_verbose = $(PYTHON_verbose_$(V))
PYTHON_verbose_ = $(PYTHON_verbose_$(AM_DEFAULT_VERBOSITY))
PYTHON_verbose_0 = @echo "  PYTHON" $@;

GPERF_verbose = $(GPERF_verbose_$(V))
GPERF_verbose_ = $(GPERF_verbose_$(AM_DEFAULT_VERBOSITY))
GPERF_verbose_0 = @echo "  GPERF " $@;
endif

if GPERF_BUILDING
CLEANFILES += src/frontend/cxx-asttype.h
endif

src/frontend/cxx-asttype.h : src/frontend/cxx-asttype.def $(NODECL_GENERATOR)
	$(AM_V_GEN)( \
    OUT_FILE="src/frontend/cxx-asttype.h";\
	echo "#ifndef CXX_ASTTYPE_H" > $${OUT_FILE}; \
	echo "#define CXX_ASTTYPE_H" >> $${OUT_FILE}; \
	echo "#include \"libmcxx-common.h\"" >> $${OUT_FILE}; \
	echo "#include \"cxx-macros.h\"" >> $${OUT_FILE}; \
	echo "MCXX_BEGIN_DECLS" >> $${OUT_FILE}; \
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" >> $${OUT_FILE}; \
	echo "enum node_type {" >> $${OUT_FILE}; \
	echo "AST_INVALID_NODE = 0,"  >> $${OUT_FILE}; \
	$(SED) -e "/^[[:blank:]]*\(#.*\)\?$$/d" -e "s/$$/,/" < src/frontend/cxx-asttype.def >> $${OUT_FILE}; \
	echo "AST_LAST_NODE" >> $${OUT_FILE}; \
	echo "};" >> $${OUT_FILE}; \
	echo "LIBMCXX_EXTERN char* ast_node_names[];" >> $${OUT_FILE}; \
	echo "typedef enum node_type node_t;" >> $${OUT_FILE}; \
	echo "MCXX_END_DECLS" >> $${OUT_FILE}; \
	echo "#endif // CXX_ASTTYPE_H" >> $${OUT_FILE}; \
    )


if GPERF_BUILDING
CLEANFILES += src/frontend/cxx-asttype.c
endif

src/frontend/cxx-asttype.c : src/frontend/cxx-asttype.def src/frontend/cxx-asttype.h
	$(AM_V_GEN)( \
    OUT_FILE="src/frontend/cxx-asttype.c";\
	echo "/* This file has been generated. Every time you change $< it will be regenerated */" > $${OUT_FILE}; \
	echo "#include \"cxx-asttype.h\"" >> $${OUT_FILE}; \
	echo "char* ast_node_names[] = {" >> $${OUT_FILE}; \
	echo "[AST_INVALID_NODE] = \"AST_INVALID_NODE\", " >> $${OUT_FILE}; \
	$(SED) -e "/^[[:blank:]]*\(#.*\)\?$$/d" -e "s/^.*$$/[&] = \"&\",/" < src/frontend/cxx-asttype.def >> $${OUT_FILE}; \
	echo "[AST_LAST_NODE] = \"AST_LAST_NODE\", " >> $${OUT_FILE}; \
	echo "};" >> $${OUT_FILE}; \
	)

BUILT_SOURCES += src/frontend/cxx-asttype.def
CLEANFILES += src/frontend/cxx-asttype.def
src/frontend/cxx-asttype.def : $(TPP) $(addprefix $(top_srcdir)/, $(AST_NODE_TYPE_FILES)) $(AST_NODE_TYPE_FILES_GEN)
	$(TPP_verbose)$(TPP) -o src/frontend/cxx-asttype.def -I$(top_srcdir)/src/frontend -I./src/frontend $(top_srcdir)/src/frontend/cxx-asttype-all.def

if GPERF_BUILDING
BUILT_SOURCES += src/frontend/cxx-asttype-str.gperf
CLEANFILES += src/frontend/cxx-asttype-str.gperf
src/frontend/cxx-asttype-str.gperf : src/frontend/cxx-asttype.def
	$(AM_V_GEN)( \
    OUT_FILE="src/frontend/cxx-asttype-str.gperf"; \
	echo "%{" > $${OUT_FILE}; \
	echo "/* This file has been generated. Every time you change $+ it will be regenerated */" >> $${OUT_FILE}; \
	echo "%}" >> $${OUT_FILE}; \
	echo "struct node_str_t;" >> $${OUT_FILE}; \
	echo "%%" >> $${OUT_FILE}; \
	echo "\"AST_INVALID_NODE\", AST_INVALID_NODE" >> $${OUT_FILE}; \
	$(SED) -e "/^[[:blank:]]*\(#.*\)\?$$/d" -e "s/^[[:blank:]]*\([^[:blank:]]\+\)[[:blank:]]*$$/\"\1\", \1/" < src/frontend/cxx-asttype.def >> $${OUT_FILE}; \
	echo "\"AST_LAST_NODE\", AST_LAST_NODE" >> $${OUT_FILE}; \
	echo "%%" >> $${OUT_FILE}; \
    )

endif

if GPERF_BUILDING
BUILT_SOURCES += src/frontend/cxx-asttype-str-internal.h
CLEANFILES += src/frontend/cxx-asttype-str-internal.h
endif

if GPERF_BUILDING
src/frontend/cxx-asttype-str-internal.h: src/frontend/cxx-asttype-str.gperf
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=ast_node_kind_hash --struct-type \
      --lookup-function-name=ast_node_name_to_kind_ --output=src/frontend/cxx-asttype-str-internal.h \
      --initializer-suffix=,AST_INVALID_NODE src/frontend/cxx-asttype-str.gperf
endif

if FLEX_BUILDING
CLEANFILES += src/frontend/cxx-scanner.c
CLEANFILES += src/frontend/cxx03.l
endif

if FLEX_BUILDING
src/frontend/cxx-scanner.c : src/frontend/cxx03.l src/frontend/cxx-parser.h
	$(FLEX_verbose)$(FLEX) -Pmcxx -d -osrc/frontend/cxx-scanner.c -8 src/frontend/cxx03.l
endif

src/frontend/cxx03.l : $(TPP) src/frontend/cxx-lexer.l
	$(TPP_verbose)(rm -f src/frontend/cxx03.l && $(TPP) -o src/frontend/cxx03.l -D CPLUSPLUS $(top_srcdir)/src/frontend/cxx-lexer.l && chmod -w src/frontend/cxx03.l)

if BISON_BUILDING
CLEANFILES += src/frontend/cxx-parser.c
CLEANFILES += src/frontend/cxx-parser-internal.h
CLEANFILES += src/frontend/cxx-parser.output
CLEANFILES += src/frontend/cxx03.y
endif

src/frontend/cxx03.y : $(TPP) $(top_srcdir)/src/frontend/cxx03.y.in $(addprefix $(top_srcdir)/, $(ADDITIONAL_GRAMMARS))
	$(TPP_verbose)(rm -f src/frontend/cxx03.y && $(TPP) -o src/frontend/cxx03.y -D CPLUSPLUS -I$(top_srcdir)/src/frontend $(top_srcdir)/src/frontend/cxx03.y.in && chmod -w src/frontend/cxx03.y)

src/frontend/cxx-parser-internal.h : src/frontend/cxx-parser.c
if BISON_BUILDING
src/frontend/cxx-parser.c : src/frontend/cxx03.y
	$(BISON_verbose)$(BISON) -pmcxx --debug --defines=src/frontend/cxx-parser-internal.h --report=all --output=src/frontend/cxx-parser.c src/frontend/cxx03.y
endif

# These are obtained when invoking bison, if we write a rule like 'a b : d' it
# breaks parallel compilations since bison gets invoked twice.
src/frontend/cxx-parser.h : src/frontend/cxx-parser.c
src/frontend/cxx-parser.output : src/frontend/cxx-parser.c

if FLEX_BUILDING
CLEANFILES += src/frontend/c99-scanner.c
CLEANFILES += src/frontend/c99.l
endif

if FLEX_BUILDING
src/frontend/c99-scanner.c : src/frontend/c99.l src/frontend/c99-parser.h
	$(FLEX_verbose)$(FLEX) -Pmc99 -d -osrc/frontend/c99-scanner.c -8 src/frontend/c99.l
endif

src/frontend/c99.l : $(TPP) $(top_srcdir)/src/frontend/cxx-lexer.l
	$(TPP_verbose)(rm -f src/frontend/c99.l && $(TPP) -o src/frontend/c99.l -D C99 $(top_srcdir)/src/frontend/cxx-lexer.l && chmod -w src/frontend/c99.l)

if BISON_BUILDING
CLEANFILES += src/frontend/c99-parser.c
CLEANFILES += src/frontend/c99-parser-internal.h
CLEANFILES += src/frontend/c99-parser.output
CLEANFILES += src/frontend/c99.y
endif

src/frontend/c99.y : $(TPP) $(top_srcdir)/src/frontend/c99.y.in $(addprefix $(top_srcdir)/, $(ADDITIONAL_GRAMMARS))
	$(TPP_verbose)(rm -f src/frontend/c99.y && $(TPP) -o src/frontend/c99.y -D C99 -I$(top_srcdir)/src/frontend $(top_srcdir)/src/frontend/c99.y.in)

src/frontend/c99-parser-internal.h : src/frontend/c99-parser.c
if BISON_BUILDING
src/frontend/c99-parser.c : src/frontend/c99.y
	$(BISON_verbose)$(BISON) -pmc99 --debug --defines=src/frontend/c99-parser-internal.h --report=all --output=src/frontend/c99-parser.c src/frontend/c99.y
endif

# These are obtained when invoking bison, if we write a rule like 'a b : d' it
# breaks parallel compilations since bison gets invoked twice.
src/frontend/c99-parser.h : src/frontend/c99-parser.c
src/frontend/c99-parser.output : src/frontend/c99-parser.c

CLEANFILES    += src/frontend/cxx-entity-specs.h
BUILT_SOURCES += src/frontend/cxx-entity-specs.h
src/frontend/cxx-entity-specs.h : $(top_srcdir)/src/frontend/cxx-entity-specs.def $(top_srcdir)/src/frontend/gen-symbol-attrs.py
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/gen-symbol-attrs.py $(top_srcdir)/src/frontend/cxx-entity-specs.def > $@

CLEANFILES    += src/frontend/cxx-entity-specs-ops.h
BUILT_SOURCES += src/frontend/cxx-entity-specs-ops.h
src/frontend/cxx-entity-specs-ops.h : $(top_srcdir)/src/frontend/cxx-entity-specs.def $(top_srcdir)/src/frontend/gen-symbol-attrs.py
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/gen-symbol-attrs.py $(top_srcdir)/src/frontend/cxx-entity-specs.def getters_and_setters > $@

CLEANFILES    += src/frontend/cxx-symbol-deep-copy-entity-specs.c
BUILT_SOURCES += src/frontend/cxx-symbol-deep-copy-entity-specs.c
src/frontend/cxx-symbol-deep-copy-entity-specs.c : $(top_srcdir)/src/frontend/cxx-entity-specs.def $(top_srcdir)/src/frontend/gen-symbol-attrs.py
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/gen-symbol-attrs.py $(top_srcdir)/src/frontend/cxx-entity-specs.def c_deep_copy_entity_specs > $@

CLEANFILES    += src/frontend/cxx-nodecl-checker.c
BUILT_SOURCES += src/frontend/cxx-nodecl-checker.c
src/frontend/cxx-nodecl-checker.c : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) > $@

CLEANFILES    += src/frontend/cxx-nodecl-output.c
BUILT_SOURCES += src/frontend/cxx-nodecl-output.c
src/frontend/cxx-nodecl-output.c : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) generation_routines_impl > $@

CLEANFILES    += src/frontend/cxx-nodecl-output.h
BUILT_SOURCES += src/frontend/cxx-nodecl-output.h
src/frontend/cxx-nodecl-output.h : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) generation_routines_header > $@

CLEANFILES    += src/frontend/cxx-nodecl-visitor.h
BUILT_SOURCES += src/frontend/cxx-nodecl-visitor.h
src/frontend/cxx-nodecl-visitor.h : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) c_visitor_decl > $@

CLEANFILES    += src/frontend/cxx-nodecl-visitor.c
BUILT_SOURCES += src/frontend/cxx-nodecl-visitor.c
src/frontend/cxx-nodecl-visitor.c : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) c_visitor_def > $@

CLEANFILES    += src/frontend/cxx-nodecl-shallow-copy.c
BUILT_SOURCES += src/frontend/cxx-nodecl-shallow-copy.c
src/frontend/cxx-nodecl-shallow-copy.c : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/nodecl-generator.py $(top_srcdir)/src/frontend/cxx-nodecl.def c_shallow_copy_def > $@

CLEANFILES    += src/frontend/cxx-nodecl-deep-copy-base.c
BUILT_SOURCES += src/frontend/cxx-nodecl-deep-copy-base.c
src/frontend/cxx-nodecl-deep-copy-base.c : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/nodecl-generator.py $(top_srcdir)/src/frontend/cxx-nodecl.def c_deep_copy_def > $@

CLEANFILES    += src/frontend/cxx-asttype-nodecl.def
BUILT_SOURCES += src/frontend/cxx-asttype-nodecl.def
src/frontend/cxx-asttype-nodecl.def : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/nodecl-generator.py $(top_srcdir)/src/frontend/cxx-nodecl.def asttype_nodecl > $@

##########################################################################
# src/frontend/fortran
##########################################################################

lib_LTLIBRARIES += src/frontend/fortran/libmf03.la src/frontend/fortran/libmf03-prescanner.la

BUILT_SOURCES += src/frontend/fortran/fortran03-parser.c
BUILT_SOURCES += src/frontend/fortran/fortran03-parser-internal.h
BUILT_SOURCES += src/frontend/fortran/fortran03.y
BUILT_SOURCES += src/frontend/fortran/prescanner-scanner.h
BUILT_SOURCES += src/frontend/fortran/fortran03-modules-bits.h
BUILT_SOURCES += src/frontend/fortran/fortran03-keywords.c

# Mercurium C/C++ compiler runtime library
src_frontend_fortran_libmf03_la_SOURCES = \
                     src/frontend/fortran/fortran03-parser.c \
                     src/frontend/fortran/fortran03-parser.h \
                     src/frontend/fortran/fortran03-lexer.h \
                     src/frontend/fortran/fortran03-keywords.c \
                     src/frontend/fortran/fortran03-scanner.h \
                     src/frontend/fortran/fortran03-scanner.c \
                     src/frontend/fortran/fortran03-parser-internal.h \
                     src/frontend/fortran/fortran03-cexpr.h \
                     src/frontend/fortran/fortran03-cexpr.c \
                     src/frontend/fortran/fortran03-utils.h \
                     src/frontend/fortran/fortran03-utils.c \
                     src/frontend/fortran/fortran03-typeutils.h \
                     src/frontend/fortran/fortran03-typeutils.c \
                     src/frontend/fortran/fortran03-typeenviron-decls.h \
                     src/frontend/fortran/fortran03-typeenviron.h \
                     src/frontend/fortran/fortran03-typeenviron.c \
                     src/frontend/fortran/fortran03-prettyprint.c \
                     src/frontend/fortran/fortran03-prettyprint.h \
                     src/frontend/fortran/fortran03-split.h \
                     src/frontend/fortran/fortran03-split.c \
                     src/frontend/fortran/fortran03-scope-decls.h \
                     src/frontend/fortran/fortran03-scope.h \
                     src/frontend/fortran/fortran03-scope.c \
                     src/frontend/fortran/fortran03-buildscope.h \
                     src/frontend/fortran/fortran03-buildscope.c \
                     src/frontend/fortran/fortran03-exprtype.h \
                     src/frontend/fortran/fortran03-exprtype.c \
                     src/frontend/fortran/fortran03-intrinsics.h \
                     src/frontend/fortran/fortran03-intrinsics-simplify.h \
                     src/frontend/fortran/fortran03-intrinsics.c \
                     src/frontend/fortran/fortran03-modules.h \
                     src/frontend/fortran/fortran03-modules-data.h \
                     src/frontend/fortran/fortran03-modules-bits.h \
                     src/frontend/fortran/fortran03-modules.c \
                     src/frontend/fortran/fortran03-codegen.h \
                     src/frontend/fortran/fortran03-mangling.h \
                     src/frontend/fortran/fortran03-mangling.c \
                     src/frontend/fortran/libmf03-common.h \
                     $(END)

src_frontend_fortran_libmf03_la_LIBADD = \
					$(top_builddir)/lib/libmcxx-utils.la \
                    $(top_builddir)/src/frontend/libmcxx-process.la \
                    $(sqlite3_LIBS) \
					$(END)

src_frontend_fortran_libmf03_la_LDFLAGS = $(dll_exports) -avoid-version $(no_undefined)

# This is a humongous amount of memory but sometimes
# lines generated by the compiler are HUGE
FORTRAN_FLEX_FLAGS=-DYY_BUF_SIZE=1048576

src_frontend_fortran_libmf03_la_CFLAGS = \
				    -std=gnu99 \
					-DLIBMF03_DLL_EXPORT \
                    -Wall \
					-Wshadow \
					-Wextra \
				    -I$(top_srcdir)/lib \
				    -I$(top_srcdir)/support/gperf \
				    -I$(top_builddir)/support/gperf \
				    -I$(top_srcdir)/src/frontend \
				    -I$(top_srcdir)/src/frontend/fortran \
				    -I$(top_srcdir)/src/mcxx_tl \
				    -I$(top_builddir)/src/frontend \
				    -I$(top_builddir)/src/frontend/fortran \
				    -I$(top_srcdir)/src/driver \
				    -I$(top_srcdir)/src/driver/fortran \
				    -I$(top_builddir)/src/driver \
                    $(FORTRAN_FLEX_FLAGS) \
                    -fexceptions \
					-std=gnu99 \
				    -DPKGDATADIR=\"$(pkgdatadir)\" \
                    $(sqlite3_CFLAGS) \
                    $(END)


# Prescanner
src_frontend_fortran_libmf03_prescanner_la_SOURCES = \
                                src/frontend/fortran/prescanner-identifier.c \
                                src/frontend/fortran/prescanner-identifier.h \
                                src/frontend/fortran/prescanner-process.c \
                                src/frontend/fortran/prescanner-process-decls.h \
                                src/frontend/fortran/prescanner-process.h \
                                src/frontend/fortran/libmf03-prescanner-common.h \
                                $(END)

src_frontend_fortran_libmf03_prescanner_la_CFLAGS = \
					-std=gnu99 \
					-DLIBMF03_PRESCANNER_DLL_EXPORT \
                    -Wall \
					-Wshadow \
					-Wextra \
				    -I$(top_srcdir)/lib \
				    -I$(top_srcdir)/support/gperf \
				    -I$(top_builddir)/support/gperf \
				    -I$(top_srcdir)/src/frontend \
				    -I$(top_srcdir)/src/frontend/fortran \
				    -I$(top_builddir)/src/frontend \
				    -I$(top_builddir)/src/frontend/fortran \
				    -I$(top_srcdir)/src/driver \
					$(FORTRAN_FLEX_FLAGS) \
                    -fexceptions \
				    -DPKGDATADIR=\"$(pkgdatadir)\"

src_frontend_fortran_libmf03_prescanner_la_LDFLAGS = $(dll_exports) -avoid-version $(no_undefined)

# Remaining stuff

ADDITIONAL_GRAMMARS_FORTRAN_ONLY =
ADDITIONAL_GRAMMARS_FORTRAN_ONLY += src/frontend/fortran/fortran03-subparse.y
ADDITIONAL_GRAMMARS_FORTRAN_ONLY += src/frontend/fortran/fortran03-omp.y
ADDITIONAL_GRAMMARS_FORTRAN_ONLY += src/frontend/fortran/fortran03-ompss.y
ADDITIONAL_GRAMMARS_FORTRAN_ONLY += src/frontend/fortran/fortran03-extensions.y

EXTRA_DIST += $(ADDITIONAL_GRAMMARS_FORTRAN_ONLY)

ADDITIONAL_GRAMMARS_FORTRAN =
ADDITIONAL_GRAMMARS_FORTRAN += $(ADDITIONAL_GRAMMARS_FORTRAN_ONLY)
ADDITIONAL_GRAMMARS_FORTRAN += src/frontend/cxx-pragma.y
ADDITIONAL_GRAMMARS_FORTRAN += src/frontend/cxx-literal-nodes.y
ADDITIONAL_GRAMMARS_FORTRAN += src/frontend/cxx-placeholders.y


EXTRA_DIST += src/frontend/fortran/fortran03.y
EXTRA_DIST += src/frontend/fortran/fortran03.y.in

EXTRA_DIST += src/frontend/fortran/prescanner.l
EXTRA_DIST += src/frontend/fortran/prescanner-scanner.h

if GPERF_BUILDING
CLEANFILES += src/frontend/fortran/fortran03-keywords.c
endif
src/frontend/fortran/fortran03-keywords.c : $(srcdir)/src/frontend/fortran/fortran03-keywords.gperf
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=fortran_keywords_hash --struct-type \
      --initializer-suffix=,0 --lookup-function-name=fortran_keywords_lookup --output=src/frontend/fortran/fortran03-keywords.c \
	  --ignore-case --compare-strncmp \
      $(srcdir)/src/frontend/fortran/fortran03-keywords.gperf
else
	@echo "*** ERROR: file fortran03-keywords.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

EXTRA_DIST += src/frontend/fortran/fortran03-keywords.gperf

src/frontend/fortran/fortran03.y : $(TPP) $(top_srcdir)/src/frontend/fortran/fortran03.y.in $(addprefix $(top_srcdir)/, $(ADDITIONAL_GRAMMARS_FORTRAN))
	$(TPP_verbose)(rm -f src/frontend/fortran/fortran03.y && $(TPP) -I$(top_srcdir)/src/frontend -I$(top_srcdir)/src/frontend/fortran -o src/frontend/fortran/fortran03.y -D FORTRAN2003 $(top_srcdir)/src/frontend/fortran/fortran03.y.in && chmod -w src/frontend/fortran/fortran03.y)

if BISON_BUILDING
CLEANFILES += src/frontend/fortran/fortran03-parser.c
CLEANFILES += src/frontend/fortran/fortran03-parser-internal.h
CLEANFILES += src/frontend/fortran/fortran03-parser.output
CLEANFILES += src/frontend/fortran/fortran03.y
endif


EXTRA_DIST += src/frontend/fortran/fortran03-parser.c

src/frontend/fortran/fortran03-parser-internal.h : src/frontend/fortran/fortran03-parser.c
if BISON_BUILDING
src/frontend/fortran/fortran03-parser.c : src/frontend/fortran/fortran03.y
	$(BISON_verbose)$(BISON) -pmf03 --debug --defines=src/frontend/fortran/fortran03-parser-internal.h --report=all --output=src/frontend/fortran/fortran03-parser.c src/frontend/fortran/fortran03.y
endif

if FLEX_BUILDING
CLEANFILES += src/frontend/fortran/prescanner-scanner.h
endif

if FLEX_BUILDING
src/frontend/fortran/prescanner-scanner.h : $(top_srcdir)/src/frontend/fortran/prescanner-identifier.h $(top_srcdir)/src/frontend/fortran/prescanner.l
	$(FLEX_verbose)$(FLEX) -Pprescanner -d -i -o src/frontend/fortran/prescanner-scanner.h -8 $(top_srcdir)/src/frontend/fortran/prescanner.l
endif

CLEANFILES += src/frontend/fortran/fortran03-modules-bits.h
src/frontend/fortran/fortran03-modules-bits.h : $(top_srcdir)/src/frontend/gen-symbol-attrs.py $(top_srcdir)/src/frontend/cxx-entity-specs.def
	$(PYTHON_verbose)$(PYTHON) $(top_srcdir)/src/frontend/gen-symbol-attrs.py $(top_srcdir)/src/frontend/cxx-entity-specs.def fortran_modules > src/frontend/fortran/fortran03-modules-bits.h


##########################################################################
# src/tl
##########################################################################

lib_LTLIBRARIES += src/tl/libtl.la

BUILT_SOURCES += src/tl/tl-lexer-tokens.hpp
BUILT_SOURCES += src/tl/tl-nodecl-visitor.hpp
BUILT_SOURCES += src/tl/tl-nodecl-visitor.cpp
BUILT_SOURCES += src/tl/tl-nodecl-fwd.hpp
BUILT_SOURCES += src/tl/tl-nodecl.hpp
BUILT_SOURCES += src/tl/tl-nodecl.cpp

CLEANFILES += src/tl/tl-lexer-tokens.hpp

TL_COMMON_CFLAGS = -DLIBTL_DLL_EXPORT -Wall \
    -I$(top_srcdir)/support/gperf \
    -I$(top_builddir)/support/gperf \
    -I $(top_srcdir)/src/tl/ \
    -I $(top_srcdir)/src/tl/codegen/common \
    -I $(top_srcdir)/src/tl/codegen/base/cxx \
    -I $(top_srcdir)/src/tl/codegen/base/fortran \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/frontend/fortran \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/src/mcxx_tl \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl/ \
    -I $(top_builddir)/src/tl/codegen/common \
    -I $(top_builddir)/src/tl/codegen/base/cxx \
    -I $(top_builddir)/src/tl/codegen/base/fortran \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/frontend/fortran \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/src/mcxx_tl \
    -I $(top_builddir)/lib \
    $(END)

src_tl_libtl_la_CFLAGS = $(TL_COMMON_CFLAGS)
src_tl_libtl_la_CXXFLAGS = $(TL_COMMON_CFLAGS)

src_tl_libtl_la_SOURCES = \
    src/tl/tl-common.hpp \
    src/tl/tl-object.hpp \
    src/tl/tl-object.cpp \
    src/tl/tl-builtin.hpp \
    src/tl/tl-builtin.cpp \
    src/tl/tl-nodecl-visitor.hpp \
    src/tl/tl-nodecl-visitor.cpp \
    src/tl/tl-nodecl-fwd.hpp \
    src/tl/tl-nodecl-base-fwd.hpp \
    src/tl/tl-nodecl-base.hpp \
    src/tl/tl-nodecl-base.cpp \
    src/tl/tl-nodecl.hpp \
    src/tl/tl-nodecl.cpp \
    src/tl/tl-nodecl-utils.hpp \
    src/tl/tl-nodecl-utils.cpp \
    src/tl/tl-nodecl-utils-fortran.hpp \
    src/tl/tl-nodecl-utils-fortran.cpp \
    src/tl/tl-nodecl-utils-c.hpp \
    src/tl/tl-nodecl-utils-c.cpp \
    src/tl/tl-scope.hpp \
    src/tl/tl-scope-fwd.hpp \
    src/tl/tl-scope.cpp \
    src/tl/tl-dto.hpp \
    src/tl/tl-dto.cpp \
    src/tl/tl-symbol-fwd.hpp \
    src/tl/tl-symbol.hpp \
    src/tl/tl-symbol.cpp \
    src/tl/tl-symbol-utils.hpp \
    src/tl/tl-symbol-utils.cpp \
    src/tl/tl-compilerphase.hpp \
    src/tl/tl-compilerphase.cpp \
    src/tl/tl-lexer.hpp \
    src/tl/tl-lexer.cpp \
    src/tl/tl-lexer-tokens.hpp \
    src/tl/tl-source-fwd.hpp \
    src/tl/tl-source.hpp \
    src/tl/tl-source.cpp \
    src/tl/tl-type-fwd.hpp \
    src/tl/tl-type.hpp \
    src/tl/tl-type.cpp \
    src/tl/tl-member-decl.hpp \
    src/tl/tl-objectlist.hpp \
    src/tl/tl-objectlist.cpp \
    src/tl/tl-externalvars.hpp \
    src/tl/tl-externalvars.cpp \
    src/tl/tl-multifile.hpp \
    src/tl/tl-multifile.cpp \
    src/tl/tl-compilerpipeline.cpp \
    src/tl/tl-compilerpipeline.hpp \
    src/tl/tl-counters.hpp \
    src/tl/tl-counters.cpp \
    src/tl/tl-setdto-phase.hpp \
    src/tl/tl-setdto-phase.cpp \
    src/tl/tl-predicate.hpp \
    src/tl/tl-predicate.cpp \
    src/tl/tl-functor.hpp \
    src/tl/tl-functor.cpp \
    src/tl/tl-handler.hpp \
    src/tl/tl-handler.cpp \
    src/tl/tl-predicateutils.hpp \
    src/tl/tl-predicateutils.cpp \
    src/tl/tl-clauses-info.hpp \
    src/tl/tl-clauses-info.cpp \
    src/tl/tl-pragmasupport.hpp \
    src/tl/tl-pragmasupport.cpp \
    src/tl/tl-modules-fwd.hpp \
    src/tl/tl-modules.hpp \
    src/tl/tl-modules.cpp \
    $(END)

PARSER_INFO_C=src/frontend/c99-parser-internal.h
PARSER_INFO_CXX=src/frontend/cxx-parser-internal.h
PARSER_INFO_FORTRAN=src/frontend/fortran/fortran03-parser-internal.h

# Flex makes things very difficult
src/tl/tl-lexer-tokens.hpp : $(PARSER_INFO_C) $(PARSER_INFO_CXX) $(PARSER_INFO_FORTRAN)
	$(AM_V_GEN)(( \
	 echo "#ifndef TL_LEXER_TOKENS_HPP" ; \
	 echo "#define TL_LEXER_TOKENS_HPP" ; \
	 echo "namespace TL {"  ; \
	 echo "namespace TokensC {"  ; \
	 $(SED) -n '/enum\s\+yytokentype/,/}/p' $(PARSER_INFO_C) ; \
	 echo "}"  ; \
	 echo "namespace TokensCXX {" ; \
	 $(SED) -n '/enum\s\+yytokentype/,/}/p' $(PARSER_INFO_CXX) ; \
	 echo "}"  ; \
	 if [ -e "$(PARSER_INFO_FORTRAN)" ]; then \
	    echo "namespace TokensFortran {"  ; \
	    $(SED) -n '/enum\s\+yytokentype/,/}/p' $(PARSER_INFO_FORTRAN) ; \
	    echo "}"  ; \
	 fi; \
	 echo "}";  \
	 echo "#endif // TL_LEXER_TOKENS_HPP" ; \
	) > src/tl/tl-lexer-tokens.hpp)

NODECL_DEPS=$(NODECL_GENERATOR) $(NODECL_DEF)

CLEANFILES += src/tl/tl-nodecl-fwd.hpp
src/tl/tl-nodecl-fwd.hpp : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) cxx_nodecl_class_fwd_header > $@
CLEANFILES += src/tl/tl-nodecl.hpp
src/tl/tl-nodecl.hpp : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) cxx_nodecl_class_header > $@
CLEANFILES += src/tl/tl-nodecl.cpp
src/tl/tl-nodecl.cpp : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) cxx_nodecl_class_impl > $@
CLEANFILES += src/tl/tl-nodecl-visitor.hpp
src/tl/tl-nodecl-visitor.hpp : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) cxx_visitor_decl > $@
CLEANFILES += src/tl/tl-nodecl-visitor.cpp
src/tl/tl-nodecl-visitor.cpp : $(NODECL_DEPS)
	$(PYTHON_verbose)$(PYTHON) $(NODECL_GENERATOR) $(NODECL_DEF) cxx_visitor_impl > $@

src_tl_libtl_la_LDFLAGS = -avoid-version $(no_undefined)
src_tl_libtl_la_LIBADD = \
	$(top_builddir)/lib/libmcxx-utils.la \
	$(top_builddir)/src/frontend/libmcxx-process.la \
	$(top_builddir)/src/frontend/libmcxx.la \
	$(top_builddir)/src/frontend/fortran/libmf03.la \
	$(END)

## TL common flags
tl_cflags= -Wall -Wshadow \
    -I$(top_srcdir)/support/gperf \
    -I$(top_builddir)/support/gperf \
    -I $(top_srcdir)/src/tl/codegen/common \
    -I $(top_srcdir)/src/tl/codegen/vector \
    -I $(top_srcdir)/src/tl/codegen/base/cxx \
    -I $(top_srcdir)/src/tl/codegen/base/fortran \
    -I $(top_srcdir)/src/tl/codegen/gpu/cuda \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/mcxx_tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/frontend/fortran \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl/codegen/common \
    -I $(top_builddir)/src/tl/codegen/vector \
    -I $(top_builddir)/src/tl/codegen/base/cxx \
    -I $(top_builddir)/src/tl/codegen/base/fortran \
    -I $(top_builddir)/src/tl/codegen/gpu/cuda \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/mcxx_tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/frontend/fortran \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib \
	$(END)


tl_cxxflags=$(tl_cflags)

tl_libadd=$(top_builddir)/src/tl/libtl.la \
        $(top_builddir)/src/frontend/libmcxx-process.la \
        $(top_builddir)/src/frontend/libmcxx.la \
		$(END)

tl_no_undefined=@WIN32_NO_UNDEFINED@

tl_ldflags=-avoid-version $(tl_no_undefined)
if WINDOWS_BUILD
else
tl_ldflags += \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/mcxx \
	-Wl,-z,origin \
	--enable-new-dtags \
	$(END)
endif

## Phases common flags

phasesdir = $(pkglibdir)
phases_cflags= $(tl_cflags)
phases_cxxflags=$(tl_cflags)
phases_libadd=$(tl_libadd)

phases_ldflags=-avoid-version $(tl_no_undefined)
if WINDOWS_BUILD
else
phases_ldflags += \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/.. \
	-Wl,-z,origin \
	--enable-new-dtags \
	$(END)
endif

phases_LTLIBRARIES =

##########################################################################
# src/tl/omp/common
##########################################################################

lib_LTLIBRARIES += src/tl/omp/common/libtlomp-common.la

src_tl_omp_common_libtlomp_common_la_CFLAGS = $(tl_cflags)
src_tl_omp_common_libtlomp_common_la_CXXFLAGS = $(tl_cxxflags)

src_tl_omp_common_libtlomp_common_la_LDFLAGS = $(tl_ldflags)
src_tl_omp_common_libtlomp_common_la_LIBADD = $(tl_libadd)

src_tl_omp_common_libtlomp_common_la_SOURCES = \
   src/tl/omp/common/tl-datareference.hpp \
   src/tl/omp/common/tl-datareference.cpp \
   $(END)

##########################################################################
# src/tl/analysis
##########################################################################

# Empty

ANALYSIS_CFLAGS=-I$(top_srcdir)/src/tl/analysis/common -I$(top_srcdir)/src/tl/analysis/pcfg \
		-I$(top_srcdir)/src/tl/optimizations
ANALYSIS_LIBADD=src/tl/analysis/common/libanalysis_utils.la src/tl/analysis/pcfg/libpcfg.la -lrt

##########################################################################
# src/tl/optimizations
##########################################################################

if BUILD_OPTIMIZATIONS
lib_LTLIBRARIES += src/tl/optimizations/libtloptimizations.la

src_tl_optimizations_libtloptimizations_la_CFLAGS = $(tl_cflags)
src_tl_optimizations_libtloptimizations_la_CXXFLAGS = $(tl_cflags)

src_tl_optimizations_libtloptimizations_la_LDFLAGS = $(tl_ldflags)
src_tl_optimizations_libtloptimizations_la_LIBADD = $(tl_libadd)

src_tl_optimizations_libtloptimizations_la_SOURCES = \
                           src/tl/optimizations/tl-optimizations.hpp \
                           src/tl/optimizations/tl-optimizations.cpp \
                           src/tl/optimizations/tl-expression-reduction.hpp \
                           src/tl/optimizations/tl-expression-reduction.cpp \
                           src/tl/optimizations/tl-nodecl-calc.hpp \
                           src/tl/optimizations/tl-nodecl-calc.cpp \
                           src/tl/optimizations/tl-strength-reduction.hpp \
                           src/tl/optimizations/tl-strength-reduction.cpp \
                           $(END)
endif

if BUILD_ANALYSIS

##########################################################################
# src/tl/analysis/common
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/common/libanalysis_utils.la

src_tl_analysis_common_libanalysis_utils_la_CFLAGS = $(tl_cflags)
src_tl_analysis_common_libanalysis_utils_la_CXXFLAGS = $(tl_cflags) \
				-I$(top_srcdir)/src/tl/optimizations
src_tl_analysis_common_libanalysis_utils_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_common_libanalysis_utils_la_LIBADD = $(tl_libadd) src/tl/libtl.la \
				src/tl/optimizations/libtloptimizations.la \
				-lrt \
				$(END)

src_tl_analysis_common_libanalysis_utils_la_SOURCES=\
				src/tl/analysis/common/tl-link-data.hpp \
				src/tl/analysis/common/tl-link-data.cpp \
				src/tl/analysis/common/tl-nodecl-replacer.hpp \
				src/tl/analysis/common/tl-nodecl-replacer.cpp \
				src/tl/analysis/common/tl-analysis-utils.hpp \
				src/tl/analysis/common/tl-analysis-utils.cpp \
				src/tl/analysis/common/tl-induction-variables-data.hpp \
				src/tl/analysis/common/tl-induction-variables-data.cpp \
				src/tl/analysis/common/tl-ranges-common.hpp \
				src/tl/analysis/common/tl-ranges-common.cpp \
				src/tl/analysis/common/tl-tribool.hpp \
				$(END)

##########################################################################
# src/tl/analysis/aliasing
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/aliasing/libaliasing.la

src_tl_analysis_aliasing_libaliasing_la_CFLAGS   = $(tl_cflags) -I$(top_srcdir)/src/tl/analysis/common
src_tl_analysis_aliasing_libaliasing_la_CXXFLAGS = $(tl_cflags) -I$(top_srcdir)/src/tl/analysis/common
src_tl_analysis_aliasing_libaliasing_la_LDFLAGS  = $(tl_ldflags)
src_tl_analysis_aliasing_libaliasing_la_LIBADD   = $(tl_libadd) src/tl/libtl.la \
                                                   src/tl/omp/common/libtlomp-common.la \
                                                   src/tl/analysis/common/libanalysis_utils.la

src_tl_analysis_aliasing_libaliasing_la_SOURCES = \
        src/tl/analysis/aliasing/tl-alias-analysis.hpp \
        src/tl/analysis/aliasing/tl-alias-analysis.cpp \
        $(END)

##########################################################################
# src/tl/analysis/pcfg
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/pcfg/libpcfg.la

PCFG_COMMON_FLAGS= $(tl_cflags) \
        $(ANALYSIS_CFLAGS) \
        -I $(srcdir)/src/tl/analysis/loops \
        -I $(srcdir)/src/tl/omp/common \
        $(END)

src_tl_analysis_pcfg_libpcfg_la_CFLAGS   = $(PCFG_COMMON_FLAGS)
src_tl_analysis_pcfg_libpcfg_la_CXXFLAGS = $(PCFG_COMMON_FLAGS) \
                                           -I $(srcdir)/src/tl/analysis/aliasing
src_tl_analysis_pcfg_libpcfg_la_LDFLAGS  = $(tl_ldflags)
src_tl_analysis_pcfg_libpcfg_la_LIBADD   = $(tl_libadd) \
                                           src/tl/libtl.la \
                                           src/tl/omp/common/libtlomp-common.la \
                                           src/tl/analysis/common/libanalysis_utils.la \
                                           src/tl/analysis/aliasing/libaliasing.la \
                                           $(END)

src_tl_analysis_pcfg_libpcfg_la_SOURCES = \
                    src/tl/analysis/pcfg/tl-pcfg-utils.hpp \
                    src/tl/analysis/pcfg/tl-pcfg-utils.cpp \
                    src/tl/analysis/pcfg/tl-edge.hpp \
                    src/tl/analysis/pcfg/tl-edge.cpp \
                    src/tl/analysis/pcfg/tl-node.hpp \
                    src/tl/analysis/pcfg/tl-node.cpp \
                    src/tl/analysis/pcfg/tl-extensible-graph.hpp \
                    src/tl/analysis/pcfg/tl-extensible-graph.cpp \
                    src/tl/analysis/pcfg/tl-pcfg-visitor.hpp \
                    src/tl/analysis/pcfg/tl-pcfg-visitor.cpp \
                    src/tl/analysis/pcfg/tl-task-sync.hpp \
                    src/tl/analysis/pcfg/tl-task-sync.cpp \
                    src/tl/analysis/pcfg/tl-dot-graph.cpp \
                    $(END)

##########################################################################
# src/tl/analysis/tasks
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/tasks/libtasks_analysis.la

src_tl_analysis_tasks_libtasks_analysis_la_CFLAGS   = $(tl_cflags)
src_tl_analysis_tasks_libtasks_analysis_la_CXXFLAGS = $(tl_cflags) \
                                                      $(ANALYSIS_CFLAGS)
src_tl_analysis_tasks_libtasks_analysis_la_LDFLAGS  = $(tl_ldflags)
src_tl_analysis_tasks_libtasks_analysis_la_LIBADD   = $(tl_libadd) \
                                                      src/tl/libtl.la \
                                                      $(ANALYSIS_LIBADD)

src_tl_analysis_tasks_libtasks_analysis_la_SOURCES = src/tl/analysis/tasks/tl-task-concurrency.hpp \
                                                     src/tl/analysis/tasks/tl-task-concurrency.cpp \
                                                     $(END)

##########################################################################
# src/tl/analysis/use_def
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/use_def/libuse_def.la

analysisdatadir = $(pkgdatadir)/analysisdata

src_tl_analysis_use_def_libuse_def_la_CFLAGS = $(tl_cflags)
src_tl_analysis_use_def_libuse_def_la_CXXFLAGS = $(tl_cflags) -DMCXX_ANALYSIS_DATA_PATH=\"$(analysisdatadir)\" \
						$(ANALYSIS_CFLAGS)
src_tl_analysis_use_def_libuse_def_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_use_def_libuse_def_la_LIBADD = $(tl_libadd) src/tl/libtl.la \
					src/tl/optimizations/libtloptimizations.la $(ANALYSIS_LIBADD)

src_tl_analysis_use_def_libuse_def_la_SOURCES = \
			src/tl/analysis/use_def/tl-use-def.hpp \
			src/tl/analysis/use_def/tl-use-def-utils.cpp \
			src/tl/analysis/use_def/tl-use-def-ipa.cpp \
                        src/tl/analysis/use_def/tl-use-def.cpp \
                        $(END)

analysisdata_DATA = \
		    src/tl/analysis/use_def/cLibraryFunctionList \
		    src/tl/analysis/use_def/cppLibraryFunctionList \
		    $(END)

##########################################################################
# src/tl/analysis/liveness
##########################################################################


lib_LTLIBRARIES += src/tl/analysis/liveness/libliveness.la

src_tl_analysis_liveness_libliveness_la_CFLAGS = $(tl_cflags)
src_tl_analysis_liveness_libliveness_la_CXXFLAGS = $(tl_cflags) \
                                                   -I $(srcdir)/src/tl/analysis/tasks \
                                                   $(ANALYSIS_CFLAGS)
src_tl_analysis_liveness_libliveness_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_liveness_libliveness_la_LIBADD = $(tl_libadd) \
						src/tl/libtl.la \
						src/tl/analysis/tasks/libtasks_analysis.la \
						$(ANALYSIS_LIBADD)

src_tl_analysis_liveness_libliveness_la_SOURCES = src/tl/analysis/liveness/tl-liveness.hpp \
                         src/tl/analysis/liveness/tl-liveness.cpp \
                         $(END)

##########################################################################
# src/tl/analysis/reach_defs
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/reach_defs/libreaching_definitions.la

src_tl_analysis_reach_defs_libreaching_definitions_la_CFLAGS = $(tl_cflags)
src_tl_analysis_reach_defs_libreaching_definitions_la_CXXFLAGS = $(tl_cflags) $(ANALYSIS_CFLAGS)
src_tl_analysis_reach_defs_libreaching_definitions_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_reach_defs_libreaching_definitions_la_LIBADD = $(tl_libadd) src/tl/libtl.la $(ANALYSIS_LIBADD)

src_tl_analysis_reach_defs_libreaching_definitions_la_SOURCES = \
			src/tl/analysis/reach_defs/tl-reaching-definitions.hpp \
			src/tl/analysis/reach_defs/tl-reaching-definitions.cpp \
			$(END)

##########################################################################
# src/tl/analysis/loops
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/loops/libloops_analysis.la

src_tl_analysis_loops_libloops_analysis_la_CFLAGS = $(tl_cflags)
src_tl_analysis_loops_libloops_analysis_la_CXXFLAGS = $(tl_cflags) $(ANALYSIS_CFLAGS) -I$(srcdir)/src/tl/analysis/use_def
src_tl_analysis_loops_libloops_analysis_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_loops_libloops_analysis_la_LIBADD = $(tl_libadd) \
							src/tl/libtl.la \
							src/tl/optimizations/libtloptimizations.la \
							$(ANALYSIS_LIBADD)

src_tl_analysis_loops_libloops_analysis_la_SOURCES = src/tl/analysis/loops/tl-iv-analysis.hpp \
                               src/tl/analysis/loops/tl-iv-analysis.cpp \
                               src/tl/analysis/loops/tl-loop-analysis.hpp \
                               src/tl/analysis/loops/tl-loop-analysis.cpp \
                               $(END)

##########################################################################
# src/tl/analysis/pointers
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/pointers/libpointer_size.la

src_tl_analysis_pointers_libpointer_size_la_CFLAGS = $(tl_cflags)
src_tl_analysis_pointers_libpointer_size_la_CXXFLAGS = $(tl_cflags) $(ANALYSIS_CFLAGS)
src_tl_analysis_pointers_libpointer_size_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_pointers_libpointer_size_la_LIBADD = $(tl_libadd) \
							src/tl/libtl.la \
							$(ANALYSIS_LIBADD)

src_tl_analysis_pointers_libpointer_size_la_SOURCES = \
                               src/tl/analysis/pointers/tl-pointer-size.hpp \
                               src/tl/analysis/pointers/tl-pointer-size.cpp \
                               $(END)

##########################################################################
# src/tl/analysis/auto-scope
##########################################################################
if BUILD_AUTO_SCOPE

lib_LTLIBRARIES += src/tl/analysis/auto-scope/libauto_scope.la

src_tl_analysis_auto_scope_libauto_scope_la_CFLAGS = $(tl_cflags)
src_tl_analysis_auto_scope_libauto_scope_la_CXXFLAGS = $(tl_cflags) \
							$(ANALYSIS_CFLAGS)
src_tl_analysis_auto_scope_libauto_scope_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_auto_scope_libauto_scope_la_LIBADD = $(tl_libadd) \
							src/tl/libtl.la \
							$(ANALYSIS_LIBADD)

src_tl_analysis_auto_scope_libauto_scope_la_SOURCES = \
                          src/tl/analysis/auto-scope/tl-auto-scope.hpp \
                          src/tl/analysis/auto-scope/tl-auto-scope.cpp \
                          $(END)

endif

EXTRA_DIST += src/tl/analysis/use_def/cLibraryFunctionList \
              src/tl/analysis/use_def/cppLibraryFunctionList

##########################################################################
# src/tl/analysis/tdg
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/tdg/libtdg.la

src_tl_analysis_tdg_libtdg_la_CFLAGS = $(tl_cflags)
src_tl_analysis_tdg_libtdg_la_CXXFLAGS = $(tl_cflags) $(ANALYSIS_CFLAGS)
src_tl_analysis_tdg_libtdg_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_tdg_libtdg_la_LIBADD = $(tl_libadd) \
					src/tl/libtl.la \
					$(ANALYSIS_LIBADD)

src_tl_analysis_tdg_libtdg_la_SOURCES = src/tl/analysis/tdg/tl-task-dependency-graph.hpp \
					src/tl/analysis/tdg/tl-task-dependency-graph.cpp \
					src/tl/analysis/tdg/tl-old-tdg.cpp \
					src/tl/analysis/tdg/tl-flow-tdg.cpp \
					src/tl/analysis/tdg/tl-tdg-utils.cpp \
					src/tl/analysis/tdg/tl-expanded-tdg.cpp \
 					src/tl/analysis/tdg/tl-tdg-dot.cpp \
					src/tl/analysis/tdg/tl-tdg-json.cpp \
					src/tl/analysis/tdg/tl-tdg-components.cpp \
					src/tl/analysis/tdg/tl-tdg-mapper.cpp \
					$(END)

##########################################################################
# src/tl/analysis/range
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/range/librange.la

src_tl_analysis_range_librange_la_CFLAGS = $(tl_cflags)
src_tl_analysis_range_librange_la_CXXFLAGS = $(tl_cflags) $(ANALYSIS_CFLAGS)
src_tl_analysis_range_librange_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_range_librange_la_LIBADD = $(tl_libadd) $(ANALYSIS_LIBADD)

src_tl_analysis_range_librange_la_SOURCES = \
				src/tl/analysis/range/tl-range-utils.hpp \
				src/tl/analysis/range/tl-range-utils.cpp \
				src/tl/analysis/range/tl-ssa.hpp \
				src/tl/analysis/range/tl-ssa.cpp \
				src/tl/analysis/range/tl-range-analysis.hpp \
				src/tl/analysis/range/tl-constraints.cpp \
				src/tl/analysis/range/tl-range-analysis.cpp \
				$(END)

##########################################################################
# src/tl/analysis/complexity
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/complexity/libcomplexity.la

src_tl_analysis_complexity_libcomplexity_la_CFLAGS= $(tl_cflags)
src_tl_analysis_complexity_libcomplexity_la_CXXFLAGS= $(tl_cflags) $(ANALYSIS_CFLAGS)
src_tl_analysis_complexity_libcomplexity_la_LDFLAGS = $(tl_ldflags)
src_tl_analysis_complexity_libcomplexity_la_LIBADD = $(tl_libadd) \
	src/tl/libtl.la \
	$(ANALYSIS_LIBADD)

src_tl_analysis_complexity_libcomplexity_la_SOURCES = \
	src/tl/analysis/complexity/tl-cyclomatic-complexity.hpp \
	src/tl/analysis/complexity/tl-cyclomatic-complexity.cpp \
	$(END)

##########################################################################
# src/tl/analysis/interface
##########################################################################

lib_LTLIBRARIES += src/tl/analysis/interface/libanalysis_interface.la

src_tl_analysis_interface_libanalysis_interface_la_CFLAGS= $(tl_cflags)
src_tl_analysis_interface_libanalysis_interface_la_CXXFLAGS= $(tl_cflags) \
				-I$(srcdir)/src/tl/ \
				-I$(srcdir)/src/tl/optimizations \
				-I$(srcdir)/src/tl/analysis/common \
				-I$(srcdir)/src/tl/analysis/aliasing \
				-I$(srcdir)/src/tl/analysis/pcfg \
				-I$(srcdir)/src/tl/analysis/pointers \
				-I$(srcdir)/src/tl/analysis/use_def \
				-I$(srcdir)/src/tl/analysis/liveness \
				-I$(srcdir)/src/tl/analysis/reach_defs \
				-I$(srcdir)/src/tl/analysis/loops \
				-I$(srcdir)/src/tl/analysis/tasks \
				-I$(srcdir)/src/tl/analysis/auto-scope \
				-I$(srcdir)/src/tl/analysis/tdg \
				-I$(srcdir)/src/tl/analysis/range \
				-I$(srcdir)/src/tl/analysis/complexity \
				-I$(srcdir)/src/tl/omp/common \
				-I$(srcdir)/src/tl/omp/core \
				$(END)

src_tl_analysis_interface_libanalysis_interface_la_LDFLAGS= $(tl_ldflags)
src_tl_analysis_interface_libanalysis_interface_la_LIBADD= \
				$(tl_libadd) \
				src/tl/libtl.la \
				src/tl/optimizations/libtloptimizations.la \
				src/tl/analysis/common/libanalysis_utils.la \
				src/tl/analysis/aliasing/libaliasing.la \
				src/tl/analysis/pcfg/libpcfg.la \
				src/tl/analysis/pointers/libpointer_size.la \
				src/tl/analysis/use_def/libuse_def.la \
				src/tl/analysis/liveness/libliveness.la \
				src/tl/analysis/reach_defs/libreaching_definitions.la \
				src/tl/analysis/loops/libloops_analysis.la \
				src/tl/analysis/tasks/libtasks_analysis.la \
				src/tl/analysis/auto-scope/libauto_scope.la \
				src/tl/analysis/tdg/libtdg.la \
				src/tl/analysis/range/librange.la \
				src/tl/analysis/complexity/libcomplexity.la \
				src/tl/omp/common/libtlomp-common.la \
				$(END)

src_tl_analysis_interface_libanalysis_interface_la_SOURCES= \
                                  src/tl/analysis/interface/tl-analysis-base.hpp \
                                  src/tl/analysis/interface/tl-analysis-base.cpp \
                                  src/tl/analysis/interface/tl-analysis-internals.hpp \
                                  src/tl/analysis/interface/tl-analysis-internals.cpp \
                                  src/tl/analysis/interface/tl-analysis-interface.hpp \
                                  src/tl/analysis/interface/tl-analysis-interface.cpp \
                                  $(END)

##########################################################################
# src/tl/analysis/test_phase
##########################################################################

phases_LTLIBRARIES += src/tl/analysis/test_phase/libtest_analysis.la

src_tl_analysis_test_phase_libtest_analysis_la_CFLAGS = $(phases_cflags)
src_tl_analysis_test_phase_libtest_analysis_la_CXXFLAGS = $(phases_cxxflags) \
							  $(ANALYSIS_CFLAGS) \
							  -I$(srcdir)/src/tl/analysis/interface \
							  -I$(srcdir)/src/tl/analysis/tdg
src_tl_analysis_test_phase_libtest_analysis_la_LDFLAGS = $(phases_ldflags)
src_tl_analysis_test_phase_libtest_analysis_la_LIBADD = $(phases_libadd) \
							$(ANALYSIS_LIBADD) \
							src/tl/analysis/interface/libanalysis_interface.la \
							src/tl/analysis/tdg/libtdg.la

src_tl_analysis_test_phase_libtest_analysis_la_SOURCES = \
                              src/tl/analysis/test_phase/tl-test-analysis-phase.hpp \
                              src/tl/analysis/test_phase/tl-test-analysis-phase.cpp \
                              $(END)

endif		# BUILD_ANALYSIS

##########################################################################
# src/tl/hlt
##########################################################################
lib_LTLIBRARIES += src/tl/hlt/libtl-hlt.la

HLT_COMMON_CFLAGS=-DLIBHLT_DLL_EXPORT \
			  -I $(srcdir)/src/tl/omp/core \
			  -I $(srcdir)/src/tl/omp/common \
			  $(END)

src_tl_hlt_libtl_hlt_la_CFLAGS=$(tl_cflags) $(HLT_COMMON_CFLAGS)
src_tl_hlt_libtl_hlt_la_CXXFLAGS=$(tl_cxxflags) $(HLT_COMMON_CFLAGS)
src_tl_hlt_libtl_hlt_la_LDFLAGS=$(tl_ldflags)
src_tl_hlt_libtl_hlt_la_LIBADD=$(tl_libadd) \
		$(top_builddir)/src/tl/omp/common/libtlomp-common.la \
		$(END)

src_tl_hlt_libtl_hlt_la_SOURCES = \
      src/tl/hlt/hlt-common.hpp \
      src/tl/hlt/hlt-utils.hpp \
      src/tl/hlt/hlt-utils.cpp \
      src/tl/hlt/hlt-transform.hpp \
      src/tl/hlt/hlt-transform.cpp \
      src/tl/hlt/hlt-loop-normalize.hpp \
      src/tl/hlt/hlt-loop-normalize.cpp \
      src/tl/hlt/hlt-loop-unroll.hpp \
      src/tl/hlt/hlt-loop-unroll.cpp \
      src/tl/hlt/hlt-loop-collapse.hpp \
      src/tl/hlt/hlt-loop-collapse.cpp \
      $(END)

phases_LTLIBRARIES += src/tl/hlt/libtl-hlt-pragma.la

src_tl_hlt_libtl_hlt_pragma_la_CFLAGS=$(phases_cflags) $(HLT_COMMON_CFLAGS)
src_tl_hlt_libtl_hlt_pragma_la_CXXFLAGS=$(phases_cxxflags) $(HLT_COMMON_CFLAGS)
src_tl_hlt_libtl_hlt_pragma_la_LIBADD=$(phases_libadd) src/tl/hlt/libtl-hlt.la
src_tl_hlt_libtl_hlt_pragma_la_LDFLAGS=$(phases_ldflags)

src_tl_hlt_libtl_hlt_pragma_la_SOURCES = \
      src/tl/hlt/hlt-pragma.cpp \
      src/tl/hlt/hlt-pragma.hpp \
      $(END)

# ##########################################################################
# # src/tl/vectorization/common
# ##########################################################################
#
if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/common/libtlvectorization-common.la

src_tl_vectorization_common_libtlvectorization_common_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_common_libtlvectorization_common_la_CXXFLAGS = $(tl_cflags) \
                              -I $(top_srcdir)/src/tl/vectorization/common \
                              -I $(top_srcdir)/src/tl/vectorization/vectorizer \
                              -I $(top_srcdir)/src/tl/analysis/interface \
                              -I $(top_srcdir)/src/tl/analysis/common \
                              -I $(top_srcdir)/src/tl/analysis/pcfg \
                              -I $(top_srcdir)/src/tl/analysis/tdg \
                              -I $(top_srcdir)/src/tl/omp/core \
                              -I $(top_srcdir)/src/tl/omp/common \
                              $(END)

src_tl_vectorization_common_libtlvectorization_common_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_common_libtlvectorization_common_la_LIBADD = $(tl_libadd) \
                          $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
                          $(top_builddir)/src/tl/analysis/interface/libanalysis_interface.la \
                          $(END)

src_tl_vectorization_common_libtlvectorization_common_la_SOURCES = \
                           src/tl/vectorization/common/tl-vectorization-common.hpp \
                           src/tl/vectorization/common/tl-vectorization-utils.hpp \
                           src/tl/vectorization/common/tl-vectorization-utils.cpp \
                           src/tl/vectorization/common/tl-vectorization-prefetcher-common.hpp \
                           src/tl/vectorization/common/tl-suitable-visitor.hpp \
                           src/tl/vectorization/common/tl-suitable-visitor.cpp \
                           src/tl/vectorization/common/tl-expression-evolution-visitor.hpp \
                           src/tl/vectorization/common/tl-expression-evolution-visitor.cpp \
                           src/tl/vectorization/common/tl-vectorization-analysis-interface.hpp \
                           src/tl/vectorization/common/tl-vectorization-analysis-interface.cpp \
                           src/tl/vectorization/common/tl-vectorization-analysis-internals.hpp \
                           src/tl/vectorization/common/tl-vectorization-analysis-internals.cpp \
                           src/tl/vectorization/common/tl-vectorization-three-addresses.hpp \
                           src/tl/vectorization/common/tl-vectorization-three-addresses.cpp \
                           src/tl/vectorization/common/tl-vector-isa-descriptor.hpp \
                           src/tl/vectorization/common/tl-vector-isa-descriptor.cpp \
                           src/tl/vectorization/common/tl-function-versioning.hpp \
                           src/tl/vectorization/common/tl-function-versioning.cpp \
                           $(END)
endif

# ##########################################################################
# # src/tl/vectorization/vectorizer
# ##########################################################################
#
if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vectorizer/libtlvectorizer.la

src_tl_vectorization_vectorizer_libtlvectorizer_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_vectorizer_libtlvectorizer_la_CXXFLAGS = $(tl_cflags) \
                              -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knc/backend \
                              -I $(top_srcdir)/src/tl/vectorization/common \
                              -I $(top_srcdir)/src/tl/vectorization/vectorizer \
                              -I $(top_srcdir)/src/tl/analysis/interface \
                              -I $(top_srcdir)/src/tl/analysis/common \
                              -I $(top_srcdir)/src/tl/analysis/pcfg \
                              -I $(top_srcdir)/src/tl/analysis/tdg \
                              -I $(top_srcdir)/src/tl/optimizations \
                              -I $(top_srcdir)/src/tl/omp/core \
                              -I $(top_srcdir)/src/tl/omp/common \
                              -I $(top_srcdir)/src/tl/hlt \
                              $(END)

src_tl_vectorization_vectorizer_libtlvectorizer_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vectorizer_libtlvectorizer_la_LIBADD = $(tl_libadd) \
                          $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
                          $(top_builddir)/src/tl/vectorization/common/libtlvectorization-common.la \
                          $(top_builddir)/src/tl/optimizations/libtloptimizations.la \
                          $(top_builddir)/src/tl/analysis/interface/libanalysis_interface.la \
                          $(top_builddir)/src/tl/hlt/libtl-hlt.la \
                          $(END)

src_tl_vectorization_vectorizer_libtlvectorizer_la_SOURCES = \
                           src/tl/vectorization/vectorizer/tl-vectorizer-loop-info.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-loop-info.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-gather-scatter-optimizer.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-gather-scatter-optimizer.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-overlap-common.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-overlap-common.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-overlap-optimizer.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-overlap-optimizer.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-prefetcher.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-prefetcher.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-environment-fwd.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-environment.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-environment.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-report.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-report.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-preprocessor.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-preprocessor.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-postprocessor.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-postprocessor.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-target-type-heuristic.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-target-type-heuristic.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-loop.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-loop.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-function.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-function.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-local-symbol.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-local-symbol.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-statement.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-statement.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-expression.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-visitor-expression.cpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-vector-reduction.hpp \
                           src/tl/vectorization/vectorizer/tl-vectorizer-vector-reduction.cpp \
                           $(END)
endif

##########################################################################
# src/tl/omp
##########################################################################

# Empty


##########################################################################
# src/tl/omp/core
##########################################################################


phases_LTLIBRARIES += src/tl/omp/core/libtlomp-core.la

src_tl_omp_core_libtlomp_core_la_CFLAGS = $(phases_cflags) \
                         -I $(srcdir)/src/tl/omp/common \
						 $(END)

src_tl_omp_core_libtlomp_core_la_CXXFLAGS = $(phases_cxxflags) \
                         -I $(srcdir)/src/tl/omp/common \
						 $(END)

src_tl_omp_core_libtlomp_core_la_LDFLAGS = $(phases_ldflags)
src_tl_omp_core_libtlomp_core_la_LIBADD = $(phases_libadd) \
		$(top_builddir)/src/tl/omp/common/libtlomp-common.la \
		$(END)

EXTRA_DIST += src/tl/omp/core/tl-omp-constructs.def \
	      src/tl/omp/core/tl-oss-constructs.def

src_tl_omp_core_libtlomp_core_la_SOURCES = \
   src/tl/omp/core/tl-omp-core.hpp \
   src/tl/omp/core/tl-omp-core.cpp \
   src/tl/omp/core/tl-omp-def-undef-macros.hpp \
   src/tl/omp/core/tl-omp.hpp \
   src/tl/omp/core/tl-omp.cpp \
   src/tl/omp/core/tl-omp-deps.hpp \
   src/tl/omp/core/tl-omp-deps.cpp \
   src/tl/omp/core/tl-omp-core-devices.cpp \
   src/tl/omp/core/tl-omp-reduction.hpp \
   src/tl/omp/core/tl-omp-reduction.cpp \
   src/tl/omp/core/tl-omp-reduction-builtin.cpp \
   src/tl/omp/core/tl-ompss.hpp \
   src/tl/omp/core/tl-ompss.cpp \
   src/tl/omp/core/tl-ompss-core.cpp \
   src/tl/omp/core/tl-ompss-target.hpp \
   src/tl/omp/core/tl-ompss-target.cpp \
   $(END)

##########################################################################
# src/tl/omp/base
##########################################################################

phases_LTLIBRARIES += src/tl/omp/base/libtlomp-base.la

src_tl_omp_base_libtlomp_base_la_CFLAGS = $(phases_cflags) \
                          $(END)

src_tl_omp_base_libtlomp_base_la_CXXFLAGS = $(phases_cxxflags) \
                         -I $(srcdir)/src/tl/omp/common \
                         -I $(srcdir)/src/tl/omp/core \
                         -I $(srcdir)/src/tl/hlt \
                         -I $(srcdir)/src/tl/vectorization/common \
                         -I $(srcdir)/src/tl/vectorization/vectorizer \
                         -I $(srcdir)/src/tl/analysis/interface\
                         -I $(srcdir)/src/tl/analysis/common \
                         -I $(srcdir)/src/tl/analysis/pcfg \
                         -I $(srcdir)/src/tl/analysis/tdg \
                         $(END)

src_tl_omp_base_libtlomp_base_la_LDFLAGS = $(phases_ldflags)
src_tl_omp_base_libtlomp_base_la_LIBADD = $(phases_libadd) \
			    $(top_builddir)/src/tl/hlt/libtl-hlt.la \
                            $(top_builddir)/src/tl/omp/core/libtlomp-core.la \
                            $(END)

if BUILD_ANALYSIS
src_tl_omp_base_libtlomp_base_la_LIBADD += $(top_builddir)/src/tl/analysis/interface/libanalysis_interface.la
endif

if BUILD_VECTORIZATION
src_tl_omp_base_libtlomp_base_la_LIBADD += $(top_builddir)/src/tl/vectorization/vectorizer/libtlvectorizer.la
endif

src_tl_omp_base_libtlomp_base_la_SOURCES = \
   src/tl/omp/base/tl-omp-base-utils.hpp \
   src/tl/omp/base/tl-omp-base-utils.cpp \
   src/tl/omp/base/tl-omp-base.hpp \
   src/tl/omp/base/tl-omp-base.cpp \
   src/tl/omp/base/tl-omp-base-hlt.cpp \
   src/tl/omp/base/tl-omp-base-devices.cpp \
   src/tl/omp/base/tl-ompss-base-task.hpp \
   src/tl/omp/base/tl-ompss-base-task.cpp \
   src/tl/omp/base/tl-ompss-base-target.cpp \
   $(END)

##########################################################################
# src/tl/omp/auto-scope
##########################################################################

if BUILD_AUTO_SCOPE

phases_LTLIBRARIES += src/tl/omp/auto-scope/libtlomp_auto_scope.la

src_tl_omp_auto_scope_libtlomp_auto_scope_la_CFLAGS = $(tl_cflags) \
                          -I $(srcdir)/src/tl/analysis/interface \
                          -I $(srcdir)/src/tl/analysis/common \
                          -I $(srcdir)/src/tl/analysis/pcfg \
                          -I $(srcdir)/src/tl/analysis/tdg \
                          -I $(srcdir)/src/tl/omp/common \
                          -I $(srcdir)/src/tl/omp/core \
                          $(END)

src_tl_omp_auto_scope_libtlomp_auto_scope_la_CXXFLAGS = $(tl_cflags) \
                          -I $(srcdir)/src/tl/analysis/interface \
                          -I $(srcdir)/src/tl/analysis/common \
                          -I $(srcdir)/src/tl/analysis/pcfg \
                          -I $(srcdir)/src/tl/analysis/tdg \
                          -I $(srcdir)/src/tl/omp/common \
                          -I $(srcdir)/src/tl/omp/core \
                          $(END)

src_tl_omp_auto_scope_libtlomp_auto_scope_la_LDFLAGS = $(tl_ldflags)
src_tl_omp_auto_scope_libtlomp_auto_scope_la_LIBADD = $(tl_libadd) \
					$(top_builddir)/src/tl/omp/common/libtlomp-common.la \
					 src/tl/analysis/interface/libanalysis_interface.la \
					 $(END)


src_tl_omp_auto_scope_libtlomp_auto_scope_la_SOURCES = \
			    src/tl/omp/auto-scope/tl-omp-auto-scope.hpp \
			    src/tl/omp/auto-scope/tl-omp-auto-scope.cpp \
			    $(END)

endif

##########################################################################
# src/tl/omp/simd
##########################################################################

if BUILD_VECTORIZATION

phases_LTLIBRARIES += src/tl/omp/simd/libtlomp-simd.la

src_tl_omp_simd_libtlomp_simd_la_CFLAGS = $(phases_cflags) \
                          $(END)

src_tl_omp_simd_libtlomp_simd_la_CXXFLAGS = $(phases_cxxflags) \
                          -I $(top_srcdir)/src/tl/optimizations \
                          -I $(top_srcdir)/src/tl/vectorization/common \
                          -I $(top_srcdir)/src/tl/vectorization/vectorizer \
                          -I $(top_srcdir)/src/tl/analysis/interface \
                          -I $(top_srcdir)/src/tl/analysis/common \
                          -I $(top_srcdir)/src/tl/analysis/pcfg \
                          -I $(top_srcdir)/src/tl/analysis/tdg \
                          -I $(top_srcdir)/src/tl/omp/core \
                          -I $(top_srcdir)/src/tl/omp/common \
                          -I $(top_srcdir)/src/tl/hlt \
                          $(END)

src_tl_omp_simd_libtlomp_simd_la_LDFLAGS = $(phases_ldflags)
src_tl_omp_simd_libtlomp_simd_la_LIBADD = $(phases_libadd) \
                          $(top_builddir)/src/tl/optimizations/libtloptimizations.la \
                          $(top_builddir)/src/tl/vectorization/vectorizer/libtlvectorizer.la \
                          $(top_builddir)/src/tl/omp/core/libtlomp-core.la \
                          $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
                          $(top_builddir)/src/tl/hlt/libtl-hlt.la \
                          $(END)


src_tl_omp_simd_libtlomp_simd_la_SOURCES = \
   src/tl/omp/simd/tl-omp-simd.hpp \
   src/tl/omp/simd/tl-omp-simd.cpp \
   src/tl/omp/simd/tl-omp-simd-visitor.hpp \
   src/tl/omp/simd/tl-omp-simd-visitor.cpp \
   src/tl/omp/simd/tl-omp-simd-clauses-processor.hpp \
   src/tl/omp/simd/tl-omp-simd-clauses-processor.cpp \
   $(END)

endif

##########################################################################
# src/tl/omp/nanos-common
##########################################################################

if BUILD_NANOS_COMMON

phases_LTLIBRARIES += src/tl/omp/nanos-common/libtlnanos-version.la

src_tl_omp_nanos_common_libtlnanos_version_la_SOURCES = \
	src/tl/omp/nanos-common/tl-nanos.hpp \
	src/tl/omp/nanos-common/tl-nanos.cpp \
	$(END)

src_tl_omp_nanos_common_libtlnanos_version_la_CFLAGS = $(phases_cflags)
src_tl_omp_nanos_common_libtlnanos_version_la_CXXFLAGS = $(phases_cxxflags)

src_tl_omp_nanos_common_libtlnanos_version_la_LDFLAGS = $(phases_ldflags)
src_tl_omp_nanos_common_libtlnanos_version_la_LIBADD = $(phases_libadd)


noinst_LTLIBRARIES += src/tl/omp/nanos-common/libtlnanos-common.la

src_tl_omp_nanos_common_libtlnanos_common_la_CXXFLAGS=$(phases_cxxflags)

src_tl_omp_nanos_common_libtlnanos_common_la_SOURCES=\
           src/tl/omp/nanos-common/tl-nanos-atomic.hpp \
           src/tl/omp/nanos-common/tl-nanos-atomic.cpp \
           src/tl/omp/nanos-common/tl-nanos-threadprivate.hpp \
           src/tl/omp/nanos-common/tl-nanos-threadprivate.cpp \
           $(END)

endif

##########################################################################
# src/tl/omp/lowering-commnon/
##########################################################################

phases_LTLIBRARIES += src/tl/omp/lowering-common/libtlomplowering-common.la

src_tl_omp_lowering_common_libtlomplowering_common_la_CFLAGS = \
    $(tl_cflags)\
    -I$(top_srcdir)/src/tl/omp/common \
    -I$(top_srcdir)/src/tl/omp/core

src_tl_omp_lowering_common_libtlomplowering_common_la_CXXFLAGS = \
    $(tl_cflags) \
    -I$(top_srcdir)/src/tl/omp/common \
    -I$(top_srcdir)/src/tl/omp/core

src_tl_omp_lowering_common_libtlomplowering_common_la_LIBADD = \
    $(tl_libadd) \
    $(top_builddir)/src/tl/omp/core/libtlomp-core.la \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la

src_tl_omp_lowering_common_libtlomplowering_common_la_LDFLAGS = $(tl_ldflags)

src_tl_omp_lowering_common_libtlomplowering_common_la_SOURCES = \
    src/tl/omp/lowering-common/tl-omp-lowering-atomics.cpp \
    src/tl/omp/lowering-common/tl-omp-lowering-atomics.hpp \
    src/tl/omp/lowering-common/tl-omp-lowering-directive-environment.cpp \
    src/tl/omp/lowering-common/tl-omp-lowering-directive-environment.hpp \
    src/tl/omp/lowering-common/tl-omp-lowering-final-stmts-generator.cpp \
    src/tl/omp/lowering-common/tl-omp-lowering-final-stmts-generator.hpp \
    src/tl/omp/lowering-common/tl-omp-lowering-utils.cpp \
    src/tl/omp/lowering-common/tl-omp-lowering-utils.hpp


##########################################################################
# src/tl/omp/nanox-nodecl
##########################################################################

if BUILD_NANOX_NODECL

phases_LTLIBRARIES += src/tl/omp/nanox-nodecl/libtlnanox-lowering.la

src_tl_omp_nanox_nodecl_libtlnanox_lowering_la_CFLAGS = $(phases_cflags) \
    -I$(srcdir)/src/tl/omp/nanos-common \
    -I$(srcdir)/src/tl/omp/core \
    -I$(srcdir)/src/tl/omp/common \
    -I$(srcdir)/src/tl/omp/lowering-common \
    -I@NANOX_INCLUDES@

src_tl_omp_nanox_nodecl_libtlnanox_lowering_la_CXXFLAGS = $(phases_cxxflags) \
    -I$(srcdir)/src/tl/omp/nanos-common \
    -I$(srcdir)/src/tl/omp/core \
    -I$(srcdir)/src/tl/omp/common \
    -I$(srcdir)/src/tl/omp/lowering-common \
    -I@NANOX_INCLUDES@

src_tl_omp_nanox_nodecl_libtlnanox_lowering_la_LIBADD = $(phases_libadd) \
    src/tl/omp/core/libtlomp-core.la \
    src/tl/omp/common/libtlomp-common.la \
    src/tl/omp/nanos-common/libtlnanos-version.la \
    src/tl/omp/lowering-common/libtlomplowering-common.la

src_tl_omp_nanox_nodecl_libtlnanox_lowering_la_LDFLAGS = $(phases_ldflags)

src_tl_omp_nanox_nodecl_libtlnanox_lowering_la_SOURCES = \
								 src/tl/omp/nanox-nodecl/tl-nanox-nodecl.cpp \
								 src/tl/omp/nanox-nodecl/tl-nanox-nodecl.hpp \
								 src/tl/omp/nanox-nodecl/tl-nanox-nodecl-prerun.cpp \
								 src/tl/omp/nanox-nodecl/tl-lowering-visitor.cpp \
								 src/tl/omp/nanox-nodecl/tl-lowering-visitor.hpp \
								 src/tl/omp/nanox-nodecl/tl-lower-arguments.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-task-common.hpp \
								 src/tl/omp/nanox-nodecl/tl-lower-task.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-task-expression.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-task-call.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-taskwait.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-taskyield.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-register.cpp \
								 src/tl/omp/nanox-nodecl/tl-outline-info.hpp \
								 src/tl/omp/nanox-nodecl/tl-outline-info.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-single.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-workshare.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-master.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-barrier.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-parallel.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-for.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-for-appendix.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-for-worksharing.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-for-slicer.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-sections.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-critical.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-flush.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-atomic.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-target.cpp \
								 src/tl/omp/nanox-nodecl/tl-lower-nanox-ocl-intrinsics.cpp \
								 src/tl/omp/nanox-nodecl/tl-parallel-spawn.cpp \
								 src/tl/omp/nanox-nodecl/tl-loop-spawn-worksharing.cpp \
								 src/tl/omp/nanox-nodecl/tl-loop-spawn-slicer.cpp \
								 src/tl/omp/nanox-nodecl/tl-reduction-openmp.cpp \
								 src/tl/omp/nanox-nodecl/tl-reduction-ompss.cpp \
								 src/tl/omp/nanox-nodecl/tl-nanox-ptr.hpp \
								 src/tl/omp/nanox-nodecl/tl-nanox-ptr.cpp \
								 src/tl/omp/nanox-nodecl/tl-nanox-array-descriptor.cpp \
								 src/tl/omp/nanox-nodecl/tl-devices.cpp \
								 src/tl/omp/nanox-nodecl/tl-devices.hpp \
								 src/tl/omp/nanox-nodecl/tl-target-information.hpp \
								 src/tl/omp/nanox-nodecl/tl-reduction-task.cpp \
								 $(END)

if DISTRIBUTE_NANOS_EXPORTS
nanoxdatadir = $(pkgdatadir)/nanos
nanoxdata_DATA = \
				 src/tl/omp/nanos-common/nanos-exports.def \
				 $(END)
endif

endif

if DISTRIBUTE_NANOS_EXPORTS
EXTRA_DIST += src/tl/omp/nanos-common/nanos-exports.def
endif

##########################################################################
# src/tl/omp/nanos-main/
##########################################################################

if BUILD_NANOX_NANOSMAIN

phases_LTLIBRARIES += src/tl/omp/nanos-main/libtlnanox-nanosmain.la

src_tl_omp_nanos_main_libtlnanox_nanosmain_la_CFLAGS = $(phases_cflags)\
			 -I$(top_srcdir)/src/tl/omp/nanox-nodecl \
			 -I$(top_srcdir)/src/tl/omp/nanos-common \
			 -I$(top_srcdir)/src/tl/omp/common \
			 -I$(top_srcdir)/src/tl/omp/core
src_tl_omp_nanos_main_libtlnanox_nanosmain_la_CXXFLAGS = $(phases_cxxflags)\
			 -I$(top_srcdir)/src/tl/omp/nanox-nodecl \
			 -I$(top_srcdir)/src/tl/omp/nanos-common \
			 -I$(top_srcdir)/src/tl/omp/common \
			 -I$(top_srcdir)/src/tl/omp/core

src_tl_omp_nanos_main_libtlnanox_nanosmain_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(END)

src_tl_omp_nanos_main_libtlnanox_nanosmain_la_LDFLAGS = $(phases_ldflags) \
	$(END)

src_tl_omp_nanos_main_libtlnanox_nanosmain_la_SOURCES = \
       src/tl/omp/nanos-main/tl-omp-nanos-main.hpp \
       src/tl/omp/nanos-main/tl-omp-nanos-main.cpp \
       $(END)
endif


##########################################################################
# src/tl/omp/nanox-nodecl/devices
##########################################################################

# Empty

device_cflags=$(phases_cflags)\
			 -I$(srcdir)/src/tl/omp/nanox-nodecl \
			 -I$(srcdir)/src/tl/omp/nanos-common \
			 -I$(srcdir)/src/tl/omp/common \
			 -I$(srcdir)/src/tl/omp/core

device_cxxflags=$(phases_cflags)\
			 -I$(srcdir)/src/tl/omp/nanox-nodecl \
			 -I$(srcdir)/src/tl/omp/nanos-common \
			 -I$(srcdir)/src/tl/omp/common \
			 -I$(srcdir)/src/tl/omp/core

devices_ldflags = $(phases_ldflags) \
                  src/tl/omp/common/libtlomp-common.la \
				  $(END)

devices_libadd = src/tl/omp/common/libtlomp-common.la \
				 $(END)

##########################################################################
# src/tl/omp/nanox-nodecl/devices/smp
##########################################################################

if BUILD_NANOX_DEVICE_SMP

phases_LTLIBRARIES += src/tl/omp/nanox-nodecl/devices/smp/libtlnanox-smp.la

src_tl_omp_nanox_nodecl_devices_smp_libtlnanox_smp_la_CFLAGS = $(device_cflags)
src_tl_omp_nanox_nodecl_devices_smp_libtlnanox_smp_la_CXXFLAGS = $(device_cxxflags)

src_tl_omp_nanox_nodecl_devices_smp_libtlnanox_smp_la_LDFLAGS = $(devices_ldflags)

src_tl_omp_nanox_nodecl_devices_smp_libtlnanox_smp_la_LIBADD = $(devices_libadd)

src_tl_omp_nanox_nodecl_devices_smp_libtlnanox_smp_la_SOURCES = \
       src/tl/omp/nanox-nodecl/devices/smp/nanox-smp.hpp \
       src/tl/omp/nanox-nodecl/devices/smp/nanox-smp.cpp \
       $(END)

endif

##########################################################################
# src/tl/omp/nanox-nodecl/devices/cuda
##########################################################################

if BUILD_NANOX_DEVICE_CUDA

phases_LTLIBRARIES += src/tl/omp/nanox-nodecl/devices/cuda/libtlnanox-cuda.la

src_tl_omp_nanox_nodecl_devices_cuda_libtlnanox_cuda_la_CFLAGS = $(device_cflags)
src_tl_omp_nanox_nodecl_devices_cuda_libtlnanox_cuda_la_CXXFLAGS = $(device_cxxflags)

src_tl_omp_nanox_nodecl_devices_cuda_libtlnanox_cuda_la_LDFLAGS = $(devices_ldflags)

src_tl_omp_nanox_nodecl_devices_cuda_libtlnanox_cuda_la_LIBADD = $(devices_libadd)

src_tl_omp_nanox_nodecl_devices_cuda_libtlnanox_cuda_la_SOURCES = \
       src/tl/omp/nanox-nodecl/devices/cuda/nanox-cuda.hpp \
       src/tl/omp/nanox-nodecl/devices/cuda/nanox-cuda.cpp \
       $(END)

endif

##########################################################################
# src/tl/omp/nanox-nodecl/devices/fpga
##########################################################################

if BUILD_NANOX_DEVICE_FPGA

phases_LTLIBRARIES += src/tl/omp/nanox-nodecl/devices/fpga/libtlnanox-fpga.la

src_tl_omp_nanox_nodecl_devices_fpga_libtlnanox_fpga_la_CFLAGS = $(device_cflags)
src_tl_omp_nanox_nodecl_devices_fpga_libtlnanox_fpga_la_CXXFLAGS = $(device_cxxflags)

src_tl_omp_nanox_nodecl_devices_fpga_libtlnanox_fpga_la_LDFLAGS = $(devices_ldflags)

src_tl_omp_nanox_nodecl_devices_fpga_libtlnanox_fpga_la_LIBADD = $(devices_libadd)

src_tl_omp_nanox_nodecl_devices_fpga_libtlnanox_fpga_la_SOURCES = \
       src/tl/omp/nanox-nodecl/devices/fpga/nanox-fpga.hpp \
       src/tl/omp/nanox-nodecl/devices/fpga/nanox-fpga.cpp \
       $(END)

endif

##########################################################################
# src/tl/omp/nanox-nodecl/devices/mpi
##########################################################################

if BUILD_NANOX_DEVICE_MPI

phases_LTLIBRARIES += src/tl/omp/nanox-nodecl/devices/mpi/libtlnanox-mpi.la

src_tl_omp_nanox_nodecl_devices_mpi_libtlnanox_mpi_la_CFLAGS = $(device_cflags)
src_tl_omp_nanox_nodecl_devices_mpi_libtlnanox_mpi_la_CXXFLAGS = $(device_cxxflags)

src_tl_omp_nanox_nodecl_devices_mpi_libtlnanox_mpi_la_LDFLAGS = $(devices_ldflags)

src_tl_omp_nanox_nodecl_devices_mpi_libtlnanox_mpi_la_LIBADD = $(devices_libadd)

src_tl_omp_nanox_nodecl_devices_mpi_libtlnanox_mpi_la_SOURCES = \
       src/tl/omp/nanox-nodecl/devices/mpi/nanox-mpi.hpp \
       src/tl/omp/nanox-nodecl/devices/mpi/nanox-mpi.cpp \
       $(END)

endif

##########################################################################
# src/tl/omp/nanox-nodecl/devices/opencl
##########################################################################

if BUILD_NANOX_DEVICE_OPENCL

phases_LTLIBRARIES += src/tl/omp/nanox-nodecl/devices/opencl/libtlnanox-opencl.la

src_tl_omp_nanox_nodecl_devices_opencl_libtlnanox_opencl_la_CFLAGS = $(device_cflags)
src_tl_omp_nanox_nodecl_devices_opencl_libtlnanox_opencl_la_CXXFLAGS = $(device_cxxflags)

src_tl_omp_nanox_nodecl_devices_opencl_libtlnanox_opencl_la_LDFLAGS = $(devices_ldflags)

src_tl_omp_nanox_nodecl_devices_opencl_libtlnanox_opencl_la_LIBADD = $(devices_libadd)

src_tl_omp_nanox_nodecl_devices_opencl_libtlnanox_opencl_la_SOURCES = \
       src/tl/omp/nanox-nodecl/devices/opencl/nanox-opencl.hpp \
       src/tl/omp/nanox-nodecl/devices/opencl/nanox-opencl.cpp \
       $(END)

endif

##########################################################################
# src/tl/omp/gomp
##########################################################################

if BUILD_OMP_GOMP

phases_LTLIBRARIES += src/tl/omp/gomp/libtlgomp-omp-lowering.la

src_tl_omp_gomp_libtlgomp_omp_lowering_la_CFLAGS= $(phases_cflags)\
													-I$(srcdir)/src/tl/omp/core \
													-I$(srcdir)/src/tl/omp/common \
													$(END)

src_tl_omp_gomp_libtlgomp_omp_lowering_la_CXXFLAGS= $(phases_cxxflags) \
													  -I$(srcdir)/src/tl/omp/core \
													  -I$(srcdir)/src/tl/omp/common \
													  $(END)

src_tl_omp_gomp_libtlgomp_omp_lowering_la_LIBADD= $(phases_libadd) \
								src/tl/omp/common/libtlomp-common.la \
								src/tl/omp/core/libtlomp-core.la \
								$(END)

src_tl_omp_gomp_libtlgomp_omp_lowering_la_LDFLAGS= $(phases_ldflags)

src_tl_omp_gomp_libtlgomp_omp_lowering_la_SOURCES=\
								   src/tl/omp/gomp/tl-omp-gomp.cpp \
								   src/tl/omp/gomp/tl-omp-gomp.hpp \
								   src/tl/omp/gomp/tl-lowering-visitor.hpp \
								   src/tl/omp/gomp/tl-lowering-visitor.cpp \
								   src/tl/omp/gomp/tl-lowering-utils.hpp \
								   src/tl/omp/gomp/tl-lowering-utils.cpp \
								   src/tl/omp/gomp/tl-lower-parallel.cpp \
								   src/tl/omp/gomp/tl-lower-taskwait.cpp \
								   src/tl/omp/gomp/tl-lower-task.cpp \
								   src/tl/omp/gomp/tl-lower-master.cpp \
								   src/tl/omp/gomp/tl-lower-single.cpp \
								   src/tl/omp/gomp/tl-lower-barrier.cpp \
								   src/tl/omp/gomp/tl-lower-critical.cpp \
								   src/tl/omp/gomp/tl-lower-for.cpp \
								   src/tl/omp/gomp/tl-lower-atomic.cpp \
								   src/tl/omp/gomp/tl-lower-reductions.hpp \
								   src/tl/omp/gomp/tl-lower-reductions.cpp \
								   $(END)

endif

##########################################################################
# src/tl/omp/intel
##########################################################################

if BUILD_OMP_INTEL

phases_LTLIBRARIES += src/tl/omp/intel/libtlintel-omp-lowering.la

src_tl_omp_intel_libtlintel_omp_lowering_la_CFLAGS= $(phases_cflags)\
													-I$(srcdir)/src/tl/omp/core \
													-I$(srcdir)/src/tl/omp/lowering-common \
													-I$(srcdir)/src/tl/omp/common \
													$(END)

src_tl_omp_intel_libtlintel_omp_lowering_la_CXXFLAGS= $(phases_cxxflags) \
													  -I$(srcdir)/src/tl/omp/core \
													  -I$(srcdir)/src/tl/omp/lowering-common \
													  -I$(srcdir)/src/tl/omp/common \
													  $(END)

src_tl_omp_intel_libtlintel_omp_lowering_la_LIBADD= $(phases_libadd) \
								src/tl/omp/common/libtlomp-common.la \
								src/tl/omp/core/libtlomp-core.la \
								src/tl/omp/lowering-common/libtlomplowering-common.la \
								$(END)

src_tl_omp_intel_libtlintel_omp_lowering_la_LDFLAGS= $(phases_ldflags)

src_tl_omp_intel_libtlintel_omp_lowering_la_SOURCES=\
								   src/tl/omp/intel/tl-omp-intel.cpp \
								   src/tl/omp/intel/tl-omp-intel.hpp \
								   src/tl/omp/intel/tl-lowering-visitor.hpp \
								   src/tl/omp/intel/tl-lowering-visitor.cpp \
								   src/tl/omp/intel/tl-lowering-utils.hpp \
								   src/tl/omp/intel/tl-lowering-utils.cpp \
								   src/tl/omp/intel/tl-lower-parallel.cpp \
								   src/tl/omp/intel/tl-lower-master.cpp \
								   src/tl/omp/intel/tl-lower-single.cpp \
								   src/tl/omp/intel/tl-lower-barrier.cpp \
								   src/tl/omp/intel/tl-lower-for.cpp \
								   src/tl/omp/intel/tl-lower-reductions.hpp \
								   src/tl/omp/intel/tl-lower-reductions.cpp \
								   src/tl/omp/intel/tl-cache-rtl-calls.hpp \
								   src/tl/omp/intel/tl-cache-rtl-calls.cpp \
								   src/tl/omp/intel/tl-lower-task-common.hpp \
								   src/tl/omp/intel/tl-lower-task.cpp \
								   src/tl/omp/intel/tl-lower-taskwait.cpp \
								   src/tl/omp/intel/tl-lower-taskgroup.cpp \
								   src/tl/omp/intel/tl-lower-taskloop.cpp \
								   src/tl/omp/intel/tl-lower-taskyield.cpp \
								   src/tl/omp/intel/tl-lower-critical.cpp \
								   $(END)

endif

##########################################################################
# src/tl/omp/lint
##########################################################################

if BUILD_OMP_LINT
phases_LTLIBRARIES += src/tl/omp/lint/libtlomp-lint.la

src_tl_omp_lint_libtlomp_lint_la_CXXFLAGS = $(phases_cxxflags) \
                          -I $(top_srcdir)/src/tl/omp/common \
                          -I $(top_srcdir)/src/tl/analysis/pcfg \
                          -I $(top_srcdir)/src/tl/analysis/common \
                          -I $(top_srcdir)/src/tl/analysis/tasks \
                          -I $(top_srcdir)/src/tl/analysis/tdg \
                          -I $(top_srcdir)/src/tl/analysis/interface\
                          $(END)

src_tl_omp_lint_libtlomp_lint_la_LDFLAGS = $(phases_ldflags)
src_tl_omp_lint_libtlomp_lint_la_LIBADD = $(phases_libadd) \
			  $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
			  $(top_builddir)/src/tl/analysis/pcfg/libpcfg.la \
			  $(top_builddir)/src/tl/analysis/tasks/libtasks_analysis.la \
			  $(top_builddir)/src/tl/analysis/tdg/libtdg.la \
			  $(top_builddir)/src/tl/analysis/interface/libanalysis_interface.la \
              $(END)


src_tl_omp_lint_libtlomp_lint_la_SOURCES = \
   src/tl/omp/lint/tl-omp-lint.hpp \
   src/tl/omp/lint/tl-omp-lint.cpp \
   $(END)
endif

##########################################################################
# src/tl/ompss/nanos6
##########################################################################

if BUILD_NANOS6

phases_LTLIBRARIES += src/tl/ompss/nanos6/libtlnanos6-lowering.la

src_tl_ompss_nanos6_libtlnanos6_lowering_la_CFLAGS = \
    $(phases_cflags) \
    -I$(top_srcdir)/src/tl/omp/core \
    -I$(top_srcdir)/src/tl/omp/common \
    -I$(top_srcdir)/src/tl/omp/lowering-common \
    -I$(top_srcdir)/src/tl/ompss/nanos6 \
    -I$(top_srcdir)/src/tl/ompss/nanos6/devices \
    $(END)

src_tl_ompss_nanos6_libtlnanos6_lowering_la_CXXFLAGS = \
    $(phases_cxxflags) \
    -I$(top_srcdir)/src/tl/omp/core \
    -I$(top_srcdir)/src/tl/omp/common \
    -I$(top_srcdir)/src/tl/omp/lowering-common \
    -I$(top_srcdir)/src/tl/ompss/nanos6 \
    -I$(top_srcdir)/src/tl/ompss/nanos6/devices \
    $(END)

src_tl_ompss_nanos6_libtlnanos6_lowering_la_LIBADD = $(phases_libadd) \
    src/tl/omp/lowering-common/libtlomplowering-common.la

src_tl_ompss_nanos6_libtlnanos6_lowering_la_LDFLAGS = $(phases_ldflags)

src_tl_ompss_nanos6_libtlnanos6_lowering_la_SOURCES = \
    src/tl/ompss/nanos6/tl-nanos6.cpp \
    src/tl/ompss/nanos6/tl-nanos6.hpp \
    src/tl/ompss/nanos6/tl-nanos6-environment-capture.cpp \
    src/tl/ompss/nanos6/tl-nanos6-environment-capture.hpp \
    src/tl/ompss/nanos6/tl-nanos6-lower.cpp \
    src/tl/ompss/nanos6/tl-nanos6-lower.hpp \
    src/tl/ompss/nanos6/tl-nanos6-task.cpp \
    src/tl/ompss/nanos6/tl-nanos6-taskcall.cpp \
    src/tl/ompss/nanos6/tl-nanos6-loop.cpp \
    src/tl/ompss/nanos6/tl-nanos6-task-properties.hpp \
    src/tl/ompss/nanos6/tl-nanos6-task-properties.cpp \
    src/tl/ompss/nanos6/tl-nanos6-support.hpp \
    src/tl/ompss/nanos6/tl-nanos6-support.cpp \
    src/tl/ompss/nanos6/tl-nanos6-fortran-load-api.cpp \
    src/tl/ompss/nanos6/tl-nanos6-fortran-support.hpp \
    src/tl/ompss/nanos6/tl-nanos6-fortran-support.cpp \
    src/tl/ompss/nanos6/tl-nanos6-taskwait.cpp \
    src/tl/ompss/nanos6/tl-nanos6-critical.cpp \
    src/tl/ompss/nanos6/tl-nanos6-atomic.cpp \
    src/tl/ompss/nanos6/tl-nanos6-interface.hpp \
    src/tl/ompss/nanos6/tl-nanos6-interface.cpp \
    src/tl/ompss/nanos6/tl-nanos6-release.cpp \
    src/tl/ompss/nanos6/devices/tl-nanos6-device-manager.hpp \
    src/tl/ompss/nanos6/devices/tl-nanos6-device-factory.hpp \
    src/tl/ompss/nanos6/devices/tl-nanos6-device.hpp \
    src/tl/ompss/nanos6/devices/tl-nanos6-device.cpp \
    src/tl/ompss/nanos6/devices/smp/tl-nanos6-smp-device.hpp \
    src/tl/ompss/nanos6/devices/smp/tl-nanos6-smp-device.cpp \
    src/tl/ompss/nanos6/devices/cuda/tl-nanos6-cuda-device.hpp \
    src/tl/ompss/nanos6/devices/cuda/tl-nanos6-cuda-device.cpp \
    $(END)

endif

##########################################################################
# src/tl/analysis/checker
##########################################################################

if BUILD_ANALYSIS
phases_LTLIBRARIES += src/tl/analysis/libanalysis_check.la

src_tl_analysis_libanalysis_check_la_CFLAGS = $(phases_cflags)
src_tl_analysis_libanalysis_check_la_CXXFLAGS = $(phases_cxxflags) \
						$(ANALYSIS_CFLAGS) \
						-I$(srcdir)/src/tl/analysis/interface \
						-I$(srcdir)/src/tl/analysis/tdg \
						-I$(srcdir)/src/tl/omp/core \
						-I$(srcdir)/src/tl/omp/lint

src_tl_analysis_libanalysis_check_la_LDFLAGS = $(phases_ldflags)
src_tl_analysis_libanalysis_check_la_LIBADD = $(phases_libadd) \
						$(ANALYSIS_LIBADD) \
						src/tl/analysis/interface/libanalysis_interface.la \
						src/tl/analysis/tdg/libtdg.la \
						src/tl/omp/lint/libtlomp-lint.la

src_tl_analysis_libanalysis_check_la_SOURCES = \
			                        src/tl/analysis/checker/tl-analysis-check-phase.hpp \
						src/tl/analysis/checker/tl-analysis-check-phase.cpp \
						$(END)
endif

##########################################################################
# src/tl/omp/example
##########################################################################

if BUILD_TL_EXAMPLES

phases_LTLIBRARIES += src/tl/omp/example/libtlomp-example.la

src_tl_omp_example_libtlomp_example_la_CXXFLAGS = $(phases_cxxflags)
src_tl_omp_example_libtlomp_example_la_SOURCES = \
						src/tl/omp/example/tl-omp-example.hpp \
						src/tl/omp/example/tl-omp-example.cpp

src_tl_omp_example_libtlomp_example_la_LIBADD = $(phases_libadd)
src_tl_omp_example_libtlomp_example_la_LDFLAGS = $(phases_ldflags)

endif

##########################################################################
# src/tl/examples
##########################################################################

# Empty

##########################################################################
# src/tl/examples/01_basic_phase
##########################################################################

EXTRA_DIST += src/tl/examples/01_basic_phase/README

if BUILD_TL_EXAMPLES

phases_LTLIBRARIES += src/tl/examples/01_basic_phase/libtl_example_basic.la

src_tl_examples_01_basic_phase_libtl_example_basic_la_CXXFLAGS = $(phases_cxxflags)
src_tl_examples_01_basic_phase_libtl_example_basic_la_SOURCES = \
						src/tl/examples/01_basic_phase/tl-example-basic.hpp \
						src/tl/examples/01_basic_phase/tl-example-basic.cpp


src_tl_examples_01_basic_phase_libtl_example_basic_la_LIBADD = $(phases_libadd)
src_tl_examples_01_basic_phase_libtl_example_basic_la_LDFLAGS = $(phases_ldflags)

endif

##########################################################################
# src/tl/examples/02_parameters
##########################################################################

EXTRA_DIST += src/tl/examples/02_parameters/README

if BUILD_TL_EXAMPLES

phases_LTLIBRARIES += src/tl/examples/02_parameters/libtl_example_parameters.la

src_tl_examples_02_parameters_libtl_example_parameters_la_CXXFLAGS = $(phases_cxxflags)

src_tl_examples_02_parameters_libtl_example_parameters_la_SOURCES = \
						src/tl/examples/02_parameters/tl-example-parameters.hpp \
						src/tl/examples/02_parameters/tl-example-parameters.cpp

src_tl_examples_02_parameters_libtl_example_parameters_la_LIBADD = $(phases_libadd)
src_tl_examples_02_parameters_libtl_example_parameters_la_LDFLAGS = $(phases_ldflags)

endif

##########################################################################
# src/tl/examples/03_visitor
##########################################################################

EXTRA_DIST += src/tl/examples/03_visitor/README

if BUILD_TL_EXAMPLES

phases_LTLIBRARIES += src/tl/examples/03_visitor/libtl_example_visitor.la

src_tl_examples_03_visitor_libtl_example_visitor_la_CXXFLAGS = $(phases_cxxflags)

src_tl_examples_03_visitor_libtl_example_visitor_la_SOURCES = \
						src/tl/examples/03_visitor/tl-example-visitor.hpp \
						src/tl/examples/03_visitor/tl-example-visitor.cpp

src_tl_examples_03_visitor_libtl_example_visitor_la_LIBADD = $(phases_libadd)
src_tl_examples_03_visitor_libtl_example_visitor_la_LDFLAGS = $(phases_ldflags)


endif

##########################################################################
# src/tl/vectorization/vector-lowering/sse
##########################################################################

vector_lowering_cflags = -I $(top_srcdir)/src/tl/vectorization/common \
                         -I $(top_srcdir)/src/tl/vectorization/vectorizer \
                         -I $(top_srcdir)/src/tl/analysis/interface\
                         -I $(top_srcdir)/src/tl/analysis/common \
                         -I $(top_srcdir)/src/tl/analysis/pcfg \
                         -I $(top_srcdir)/src/tl/analysis/tdg \
                         -I $(top_srcdir)/src/tl/omp/core \
                         -I $(top_srcdir)/src/tl/omp/common \
                         -I $(top_srcdir)/src/tl/optimizations \
                         $(END)

if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vector-lowering/sse/libtlvector-lowering-sse.la

src_tl_vectorization_vector_lowering_sse_libtlvector_lowering_sse_la_CFLAGS = $(tl_cflags)
src_tl_vectorization_vector_lowering_sse_libtlvector_lowering_sse_la_CXXFLAGS = $(tl_cflags) \
    $(vector_lowering_cflags) \
    $(END)
src_tl_vectorization_vector_lowering_sse_libtlvector_lowering_sse_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vector_lowering_sse_libtlvector_lowering_sse_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(END)

src_tl_vectorization_vector_lowering_sse_libtlvector_lowering_sse_la_SOURCES = \
     src/tl/vectorization/vector-lowering/sse/legalization/tl-vector-legalization-sse.hpp \
     src/tl/vectorization/vector-lowering/sse/legalization/tl-vector-legalization-sse.cpp \
	 src/tl/vectorization/vector-lowering/sse/backend/tl-vector-backend-sse.hpp \
	 src/tl/vectorization/vector-lowering/sse/backend/tl-vector-backend-sse.cpp \
  	 $(END)
endif

##########################################################################
# src/tl/vectorization/vector-lowering/avx2
##########################################################################

if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vector-lowering/avx2/libtlvector-lowering-avx2.la

src_tl_vectorization_vector_lowering_avx2_libtlvector_lowering_avx2_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_vector_lowering_avx2_libtlvector_lowering_avx2_la_CXXFLAGS = $(tl_cflags) \
    $(vector_lowering_cflags) \
    $(END)

src_tl_vectorization_vector_lowering_avx2_libtlvector_lowering_avx2_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vector_lowering_avx2_libtlvector_lowering_avx2_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(top_builddir)/src/tl/vectorization/common/libtlvectorization-common.la \
$(END)

src_tl_vectorization_vector_lowering_avx2_libtlvector_lowering_avx2_la_SOURCES = \
     src/tl/vectorization/vector-lowering/avx2/legalization/tl-vector-legalization-avx2.hpp \
     src/tl/vectorization/vector-lowering/avx2/legalization/tl-vector-legalization-avx2.cpp \
     src/tl/vectorization/vector-lowering/avx2/backend/tl-vector-backend-avx2.hpp \
     src/tl/vectorization/vector-lowering/avx2/backend/tl-vector-backend-avx2.cpp \
     $(END)
endif

##########################################################################
# src/tl/vectorization/vector-lowering/knc
##########################################################################

if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vector-lowering/knc/libtlvector-lowering-knc.la

src_tl_vectorization_vector_lowering_knc_libtlvector_lowering_knc_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_vector_lowering_knc_libtlvector_lowering_knc_la_CXXFLAGS = $(tl_cflags) \
                              $(vector_lowering_cflags) \
                              $(END)

src_tl_vectorization_vector_lowering_knc_libtlvector_lowering_knc_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vector_lowering_knc_libtlvector_lowering_knc_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(top_builddir)/src/tl/vectorization/common/libtlvectorization-common.la \
$(END)

src_tl_vectorization_vector_lowering_knc_libtlvector_lowering_knc_la_SOURCES = \
     src/tl/vectorization/vector-lowering/knc/legalization/tl-vector-legalization-knc.hpp \
     src/tl/vectorization/vector-lowering/knc/legalization/tl-vector-legalization-knc.cpp \
     src/tl/vectorization/vector-lowering/knc/backend/tl-vector-backend-knc.hpp \
     src/tl/vectorization/vector-lowering/knc/backend/tl-vector-backend-knc-extern.hpp \
     src/tl/vectorization/vector-lowering/knc/backend/tl-vector-backend-knc.cpp \
     $(END)
endif

##########################################################################
# src/tl/vectorization/vector-lowering/knl
##########################################################################

if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vector-lowering/knl/libtlvector-lowering-knl.la

src_tl_vectorization_vector_lowering_knl_libtlvector_lowering_knl_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_vector_lowering_knl_libtlvector_lowering_knl_la_CXXFLAGS = $(tl_cflags) \
                              $(vector_lowering_cflags) \
                              -I$(top_srcdir)/src/tl/vectorization/vector-lowering/knc/legalization \
                              -I$(top_srcdir)/src/tl/vectorization/vector-lowering/knc/backend \
                              $(END)

src_tl_vectorization_vector_lowering_knl_libtlvector_lowering_knl_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vector_lowering_knl_libtlvector_lowering_knl_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(top_builddir)/src/tl/vectorization/common/libtlvectorization-common.la \
$(END)

src_tl_vectorization_vector_lowering_knl_libtlvector_lowering_knl_la_SOURCES = \
     src/tl/vectorization/vector-lowering/knl/legalization/tl-vector-legalization-knl.hpp \
     src/tl/vectorization/vector-lowering/knl/legalization/tl-vector-legalization-knl.cpp \
     src/tl/vectorization/vector-lowering/knl/backend/tl-vector-backend-knl.hpp \
     src/tl/vectorization/vector-lowering/knl/backend/tl-vector-backend-knl.cpp \
     $(END)
endif

##########################################################################
# src/tl/vectorization/vector-lowering/neon
##########################################################################

if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vector-lowering/neon/libtlvector-lowering-neon.la

src_tl_vectorization_vector_lowering_neon_libtlvector_lowering_neon_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_vector_lowering_neon_libtlvector_lowering_neon_la_CXXFLAGS = $(tl_cflags) \
                              $(vector_lowering_cflags) \
                              -I$(top_srcdir)/src/tl/vectorization/vector-lowering/neon/legalization \
                              -I$(top_srcdir)/src/tl/vectorization/vector-lowering/neon/backend \
                              $(END)

src_tl_vectorization_vector_lowering_neon_libtlvector_lowering_neon_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vector_lowering_neon_libtlvector_lowering_neon_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(top_builddir)/src/tl/vectorization/common/libtlvectorization-common.la \
$(END)

src_tl_vectorization_vector_lowering_neon_libtlvector_lowering_neon_la_SOURCES = \
     src/tl/vectorization/vector-lowering/neon/legalization/tl-vector-legalization-neon.hpp \
     src/tl/vectorization/vector-lowering/neon/legalization/tl-vector-legalization-neon.cpp \
     src/tl/vectorization/vector-lowering/neon/backend/tl-vector-backend-neon.hpp \
     src/tl/vectorization/vector-lowering/neon/backend/tl-vector-backend-neon.cpp \
     $(END)
endif

##########################################################################
# src/tl/vectorization/vector-lowering/romol
##########################################################################

if BUILD_VECTORIZATION
lib_LTLIBRARIES += src/tl/vectorization/vector-lowering/romol/libtlvector-lowering-romol.la

src_tl_vectorization_vector_lowering_romol_libtlvector_lowering_romol_la_CFLAGS = $(tl_cflags)

src_tl_vectorization_vector_lowering_romol_libtlvector_lowering_romol_la_CXXFLAGS = $(tl_cflags) \
                              $(vector_lowering_cflags) \
                              -I$(top_srcdir)/src/tl/vectorization/vector-lowering/romol/legalization \
                              -I$(top_srcdir)/src/tl/vectorization/vector-lowering/romol/backend \
                              $(END)

src_tl_vectorization_vector_lowering_romol_libtlvector_lowering_romol_la_LDFLAGS = $(tl_ldflags)
src_tl_vectorization_vector_lowering_romol_libtlvector_lowering_romol_la_LIBADD = \
    $(top_builddir)/src/tl/omp/common/libtlomp-common.la \
	$(top_builddir)/src/tl/vectorization/common/libtlvectorization-common.la \
$(END)

src_tl_vectorization_vector_lowering_romol_libtlvector_lowering_romol_la_SOURCES = \
     src/tl/vectorization/vector-lowering/romol/legalization/tl-vector-legalization-romol.hpp \
     src/tl/vectorization/vector-lowering/romol/legalization/tl-vector-legalization-romol.cpp \
     src/tl/vectorization/vector-lowering/romol/backend/tl-vector-backend-romol.hpp \
     src/tl/vectorization/vector-lowering/romol/backend/tl-vector-backend-romol.cpp \
     src/tl/vectorization/vector-lowering/romol/regalloc/tl-vector-romol-regalloc.hpp \
     src/tl/vectorization/vector-lowering/romol/regalloc/tl-vector-romol-regalloc.cpp \
     $(END)
endif

##########################################################################
# src/tl/vectorization/vector-lowering
##########################################################################

if BUILD_VECTORIZATION
phases_LTLIBRARIES += src/tl/vectorization/vector-lowering/libtlvector-lowering.la

src_tl_vectorization_vector_lowering_libtlvector_lowering_la_CFLAGS = $(phases_cflags)

src_tl_vectorization_vector_lowering_libtlvector_lowering_la_CXXFLAGS=$(phases_cxxflags)\
                 $(vector_lowering_cflags) \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/sse/ \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/sse/legalization \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/sse/backend \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/avx2/ \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/avx2/legalization \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/avx2/backend \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knc/ \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knc/legalization \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knc/backend \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knl/ \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knl/legalization \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/knl/backend \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/neon/ \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/neon/legalization \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/neon/backend \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/romol/ \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/romol/legalization \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/romol/backend \
                 -I $(top_srcdir)/src/tl/vectorization/vector-lowering/romol/regalloc \
                 $(END)

src_tl_vectorization_vector_lowering_libtlvector_lowering_la_LDFLAGS = $(phases_ldflags)

src_tl_vectorization_vector_lowering_libtlvector_lowering_la_SOURCES = \
    src/tl/vectorization/vector-lowering/tl-vector-lowering.cpp \
    src/tl/vectorization/vector-lowering/tl-vector-lowering.hpp \
	$(END)

src_tl_vectorization_vector_lowering_libtlvector_lowering_la_LIBADD = \
    $(top_builddir)/src/tl/vectorization/vector-lowering/sse/libtlvector-lowering-sse.la \
    $(top_builddir)/src/tl/vectorization/vector-lowering/avx2/libtlvector-lowering-avx2.la \
    $(top_builddir)/src/tl/vectorization/vector-lowering/knc/libtlvector-lowering-knc.la \
    $(top_builddir)/src/tl/vectorization/vector-lowering/knl/libtlvector-lowering-knl.la \
    $(top_builddir)/src/tl/vectorization/vector-lowering/neon/libtlvector-lowering-neon.la \
    $(top_builddir)/src/tl/vectorization/vector-lowering/romol/libtlvector-lowering-romol.la \
    $(END)
endif

if BUILD_TCL
##########################################################################
# src/tl/checkpoint/base
##########################################################################

phases_LTLIBRARIES += src/tl/checkpoint/base/libtlcheckpoint-base.la

src_tl_checkpoint_base_libtlcheckpoint_base_la_CFLAGS = $(phases_cflags)

src_tl_checkpoint_base_libtlcheckpoint_base_la_CXXFLAGS = $(phases_cxxflags)

src_tl_checkpoint_base_libtlcheckpoint_base_la_LDFLAGS = $(phases_ldflags)
src_tl_checkpoint_base_libtlcheckpoint_base_la_LIBADD = $(phases_libadd)


src_tl_checkpoint_base_libtlcheckpoint_base_la_SOURCES = \
   src/tl/checkpoint/base/tl-checkpoint-base.hpp \
   src/tl/checkpoint/base/tl-checkpoint-base.cpp \
   $(END)

##########################################################################
# src/tl/checkpoint/lowering
##########################################################################

phases_LTLIBRARIES += src/tl/checkpoint/lowering/libtlcheckpoint-lowering.la

src_tl_checkpoint_lowering_libtlcheckpoint_lowering_la_CFLAGS = $(phases_cflags) \
    -I$(srcdir)/src/tl/omp/common \
    -I$(srcdir)/src/tl/omp/lowering-common \
    $(END)

src_tl_checkpoint_lowering_libtlcheckpoint_lowering_la_CXXFLAGS = $(phases_cxxflags) \
    -I$(srcdir)/src/tl/omp/common \
    -I$(srcdir)/src/tl/omp/lowering-common \
    $(END)

src_tl_checkpoint_lowering_libtlcheckpoint_lowering_la_LDFLAGS = $(phases_ldflags)
src_tl_checkpoint_lowering_libtlcheckpoint_lowering_la_LIBADD = $(phases_libadd) \
    src/tl/omp/common/libtlomp-common.la \
    src/tl/omp/lowering-common/libtlomplowering-common.la \
    $(END)


src_tl_checkpoint_lowering_libtlcheckpoint_lowering_la_SOURCES = \
   src/tl/checkpoint/lowering/tl-checkpoint-lowering.hpp \
   src/tl/checkpoint/lowering/tl-checkpoint-lowering.cpp \
   src/tl/checkpoint/lowering/tl-checkpoint-visitor.hpp \
   src/tl/checkpoint/lowering/tl-checkpoint-visitor.cpp \
   $(END)
endif


##########################################################################
# src/tl/codegen
##########################################################################

# Empty

##########################################################################
# src/tl/codegen/common
##########################################################################

lib_LTLIBRARIES += src/tl/codegen/common/libcodegen-common.la

src_tl_codegen_common_libcodegen_common_la_CFLAGS=$(tl_cflags)
src_tl_codegen_common_libcodegen_common_la_CXXFLAGS=$(tl_cflags)
src_tl_codegen_common_libcodegen_common_la_LDFLAGS = $(tl_ldflags)
src_tl_codegen_common_libcodegen_common_la_SOURCES = \
    src/tl/codegen/common/codegen-common.hpp \
    src/tl/codegen/common/codegen-common.cpp \
    src/tl/codegen/common/codegen-phase.cpp \
    src/tl/codegen/common/codegen-phase.hpp \
	$(END)

##########################################################################
# src/tl/codegen/base
##########################################################################

# Empty

##########################################################################
# src/tl/codegen/base/cxx
##########################################################################

phases_LTLIBRARIES += src/tl/codegen/base/cxx/libcodegen-cxx.la

src_tl_codegen_base_cxx_libcodegen_cxx_la_CFLAGS = $(phases_cflags) -I$(srcdir)/src/tl/codegen/common
src_tl_codegen_base_cxx_libcodegen_cxx_la_CXXFLAGS = $(phases_cxxflags) -I$(srcdir)/src/tl/codegen/common

src_tl_codegen_base_cxx_libcodegen_cxx_la_LDFLAGS = $(phases_ldflags)
src_tl_codegen_base_cxx_libcodegen_cxx_la_LIBADD = src/tl/codegen/common/libcodegen-common.la \
						   $(phases_libadd) \
					       $(quadmath_LIBS) \
					       $(END)


src_tl_codegen_base_cxx_libcodegen_cxx_la_SOURCES = \
	 src/tl/codegen/base/cxx/codegen-cxx.hpp \
	 src/tl/codegen/base/cxx/codegen-cxx.cpp \
	 src/tl/codegen/base/cxx/codegen-prune.cpp \
	 src/tl/codegen/base/cxx/codegen-prune.hpp \
  	 $(END)

##########################################################################
# src/tl/codegen/base/fortran
##########################################################################

phases_LTLIBRARIES += src/tl/codegen/base/fortran/libcodegen-fortran.la

src_tl_codegen_base_fortran_libcodegen_fortran_la_CFLAGS = $(phases_cflags) \
							   -I$(top_srcdir)/src/frontend \
							   -I$(top_srcdir)/src/frontend/fortran \
							   -I$(top_builddir)/src/frontend \
							   -I$(top_builddir)/src/frontend/fortran \
							   -I$(srcdir)/src/tl/codegen/common
src_tl_codegen_base_fortran_libcodegen_fortran_la_CXXFLAGS = $(phases_cxxflags) \
							   -I$(top_srcdir)/src/frontend \
							   -I$(top_srcdir)/src/frontend/fortran \
							   -I$(top_builddir)/src/frontend \
							   -I$(top_builddir)/src/frontend/fortran \
						       -I$(srcdir)/src/tl/codegen/common

src_tl_codegen_base_fortran_libcodegen_fortran_la_LDFLAGS = $(phases_ldflags)
src_tl_codegen_base_fortran_libcodegen_fortran_la_LIBADD = src/tl/codegen/common/libcodegen-common.la \
						   $(phases_libadd) \
						   $(END)

src_tl_codegen_base_fortran_libcodegen_fortran_la_SOURCES = \
	 src/tl/codegen/base/fortran/codegen-fortran.hpp \
	 src/tl/codegen/base/fortran/codegen-fortran.cpp \
  	 $(END)

##########################################################################
# src/tl/codegen/gpu
##########################################################################

# Empty

##########################################################################
# src/tl/codegen/gpu/cuda
##########################################################################

phases_LTLIBRARIES += src/tl/codegen/gpu/cuda/libcodegen-cuda.la

src_tl_codegen_gpu_cuda_libcodegen_cuda_la_CFLAGS = $(phases_cflags) -I$(srcdir)/src/tl/codegen/common
src_tl_codegen_gpu_cuda_libcodegen_cuda_la_CXXFLAGS = $(phases_cxxflags) -I$(srcdir)/src/tl/codegen/common

src_tl_codegen_gpu_cuda_libcodegen_cuda_la_LDFLAGS = $(phases_ldflags)
src_tl_codegen_gpu_cuda_libcodegen_cuda_la_LIBADD = src/tl/codegen/common/libcodegen-common.la \
						   $(phases_libadd) \
					       $(quadmath_LIBS) \
					       $(END)


src_tl_codegen_gpu_cuda_libcodegen_cuda_la_SOURCES = \
	 src/tl/codegen/gpu/cuda/codegen-cuda.hpp \
	 src/tl/codegen/gpu/cuda/codegen-cuda.cpp \
  	 $(END)

##########################################################################
# src/mcxx_tl
##########################################################################

lib_LTLIBRARIES += src/mcxx_tl/libmcxxtl.la

src_mcxx_tl_libmcxxtl_la_SOURCES = \
	src/mcxx_tl/cxx-compilerphases.hpp \
	src/mcxx_tl/cxx-compilerphases.cpp

MCXX_TL_COMMON_CFLAGS= -DLIBMCXXTL_DLL_EXPORT \
    -I $(top_srcdir)/support/gperf \
    -I $(top_builddir)/support/gperf \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/frontend/fortran \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/tl/codegen/common \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/frontend/fortran \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/lib

src_mcxx_tl_libmcxxtl_la_CXXFLAGS = $(MCXX_TL_COMMON_CFLAGS)

src_mcxx_tl_libmcxxtl_la_CFLAGS = $(MCXX_TL_COMMON_CFLAGS)

src_mcxx_tl_libmcxxtl_la_LIBADD = \
        $(top_builddir)/src/frontend/libmcxx-process.la \
        $(top_builddir)/src/frontend/libmcxx.la \
        $(top_builddir)/src/tl/libtl.la \
        $(top_builddir)/src/tl/codegen/common/libcodegen-common.la \
	    $(top_builddir)/lib/libmcxx-utils.la \
        $(LIBADD_DLOPEN)
		$(END)

src_mcxx_tl_libmcxxtl_la_LDFLAGS=-avoid-version $(no_undefined) \
    -Wl,@RPATH_PARAMETER@,$(pkglibdir) \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/mcxx \
    -Wl,-z,origin \
	--enable-new-dtags \
    $(END)

##########################################################################
# src/driver
##########################################################################

if ENABLE_PKGLIB_INSTALL
mcxxexecdir = $(pkglibdir)
else
mcxxexecdir = $(bindir)
endif

mcxxexec_PROGRAMS =

mcxxexec_PROGRAMS += src/driver/plaincxx \
					src/driver/mcxx-embedder \
					$(END)

# _GNU_SOURCE is needed because of strsignal
# although it is available directly since glibc >=2.10
src_driver_plaincxx_CFLAGS= \
		    -std=gnu99 \
			-D_GNU_SOURCE \
			-fexceptions \
			-I$(top_srcdir)/lib \
			-I$(top_srcdir)/src/frontend \
			-I$(top_srcdir)/src/frontend/fortran \
			-I$(top_srcdir)/src/mcxx_tl \
			-I$(top_builddir)/lib \
			-I$(top_builddir)/src/frontend \
			-I$(top_builddir)/src/frontend/fortran \
			-I$(top_builddir)/src/mcxx_tl \
			-DPKGDATADIR=\"$(pkgdatadir)\" \
			-Wall

src_driver_plaincxx_CFLAGS += \
			      -I$(top_srcdir)/support/gperf \
			      -I$(top_builddir)/support/gperf \
			      -I$(top_srcdir)/src/frontend/fortran \
			      -I$(top_builddir)/src/frontend/fortran \
                              -I$(top_srcdir)/src/driver \
                              -I$(top_srcdir)/src/driver/fortran \
                              -I$(top_builddir)/src/driver \
                              -I$(top_builddir)/src/driver/fortran


src_driver_plaincxx_SOURCES = \
  src/driver/cxx-configfile.h \
  src/driver/cxx-configfile.c \
  src/driver/cxx-configoptions.c \
  src/driver/cxx-parameters.c \
  src/driver/cxx-parameters.h \
  src/driver/cxx-fileextensions.c \
  src/driver/cxx-debugflags.c \
  src/driver/cxx-driver.c \
  src/driver/cxx-driver-build-info.h \
  src/driver/cxx-driver-decls.h \
  src/driver/cxx-driver.h \
  src/driver/cxx-driver-utils.c \
  src/driver/cxx-driver-utils.h \
  src/driver/cxx-profile.c \
  src/driver/cxx-profile.h \
  src/driver/cxx-configfile-parser-internal.h \
  src/driver/cxx-configfile-parser.h \
  src/driver/cxx-configfile-parser.c \
  src/driver/cxx-configfile-lexer.h \
  src/driver/cxx-configfile-lexer.c \
  src/driver/cxx-multifile.h \
  src/driver/cxx-target-tools.h \
  src/driver/cxx-multifile.c \
  src/driver/cxx-embed.c \
  src/driver/cxx-embed.h \
  $(END)

src_driver_plaincxx_LDADD = \
   $(top_builddir)/gnulib/libgnulib.la \
   $(top_builddir)/lib/libmcxx-utils.la \
   $(top_builddir)/src/frontend/libmcxx-process.la \
   $(top_builddir)/src/frontend/libmcxx.la \
   $(top_builddir)/src/tl/libtl.la \
   $(top_builddir)/src/tl/codegen/common/libcodegen-common.la \
   $(top_builddir)/src/tl/codegen/base/cxx/libcodegen-cxx.la \
   $(top_builddir)/src/tl/codegen/base/fortran/libcodegen-fortran.la \
   $(top_builddir)/src/mcxx_tl/libmcxxtl.la \
   $(END)

src_driver_plaincxx_LDADD += $(top_builddir)/src/frontend/fortran/libmf03.la
src_driver_plaincxx_LDADD += $(top_builddir)/src/driver/fortran/libmf03-driver.la
src_driver_plaincxx_LDADD += $(sqlite3_LIBS)
src_driver_plaincxx_LDADD += $(quadmath_LIBS)
src_driver_plaincxx_LDADD += -lm

if WINDOWS_BUILD
else
# '$$ORIGIN' is a relocatable hint to GNU ld
src_driver_plaincxx_LDFLAGS = \
	-Wl,--no-as-needed \
    -Wl,-E \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/../lib \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/../lib/mcxx \
    -Wl,-z,origin \
	-Wl,--enable-new-dtags \
    $(END)
endif

EXTRA_DIST += src/driver/cxx-configoptions.gperf
EXTRA_DIST += src/driver/cxx-fileextensions.gperf
EXTRA_DIST += src/driver/cxx-debugflags.gperf

EXTRA_DIST += src/driver/cxx-configfile-parser.y
EXTRA_DIST += src/driver/cxx-configfile-lexer.l

EXTRA_DIST += src/driver/cxx-configfile-parser.c
EXTRA_DIST += src/driver/cxx-configfile-parser-internal.h
EXTRA_DIST += src/driver/cxx-configfile-lexer.c

BUILT_SOURCES += src/driver/cxx-configoptions.c
BUILT_SOURCES += src/driver/cxx-fileextensions.c
BUILT_SOURCES += src/driver/cxx-debugflags.c

BUILT_SOURCES += src/driver/cxx-configfile-parser.c
BUILT_SOURCES += src/driver/cxx-configfile-parser-internal.h
BUILT_SOURCES += src/driver/cxx-configfile-lexer.c

BUILT_SOURCES += src/driver/cxx-target-tools.h

if GPERF_BUILDING
CLEANFILES += src/driver/cxx-configoptions.c
CLEANFILES += src/driver/cxx-debugflags.c
CLEANFILES += src/driver/cxx-fileextensions.c
endif

if FLEX_BUILDING
CLEANFILES += src/driver/cxx-configfile-lexer.c
endif

if BISON_BUILDING
CLEANFILES += src/driver/cxx-configfile-parser.c
CLEANFILES += src/driver/cxx-configfile-parser-internal.h
CLEANFILES += src/driver/cxx-configfile-parser.output
endif

CLEANFILES += src/driver/cxx-target-tools.h

src/driver/cxx-configoptions.c : $(srcdir)/src/driver/cxx-configoptions.gperf $(top_srcdir)/src/driver/cxx-driver.h $(srcdir)/src/driver/cxx-configfile.h
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=src/driver/cxx-configoptions.c \
      --initializer-suffix=,NULL $(srcdir)/src/driver/cxx-configoptions.gperf
else
	@echo "*** ERROR: file cxx-configoptions.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

src/driver/cxx-fileextensions.c : $(srcdir)/src/driver/cxx-fileextensions.gperf $(top_srcdir)/src/driver/cxx-driver.h
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=fileextensions_hash --struct-type \
      --lookup-function-name=fileextensions_lookup --output=src/driver/cxx-fileextensions.c \
      --initializer-suffix=,SOURCE_LANGUAGE_UNKNOWN,SOURCE_KIND_UNKNOWN $(srcdir)/src/driver/cxx-fileextensions.gperf
else
	@echo "*** ERROR: file cxx-fileextensions.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

src/driver/cxx-debugflags.c : $(srcdir)/src/driver/cxx-debugflags.gperf $(top_srcdir)/src/driver/cxx-driver.h $(top_srcdir)/src/frontend/cxx-utils.h
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=debugflags_hash --struct-type \
      --lookup-function-name=debugflags_lookup --output=src/driver/cxx-debugflags.c \
      --global-table --word-array-name=debugflags_list \
	  "--initializer-suffix=,0,\"\"" $(srcdir)/src/driver/cxx-debugflags.gperf
else
	@echo "*** ERROR: file cxx-debugflags.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

src/driver/cxx-configfile-lexer.c: $(srcdir)/src/driver/cxx-configfile-lexer.l
if FLEX_BUILDING
	$(FLEX_verbose)$(FLEX) -Pconfigfile -d -o src/driver/cxx-configfile-lexer.c -8 $(srcdir)/src/driver/cxx-configfile-lexer.l
else
	@echo "*** ERROR: file cxx-configfile-lexer.l was modified but no suitable flex was found during configure ***"
	@exit 1
endif

src/driver/cxx-configfile-parser-internal.h : src/driver/cxx-configfile-parser.c

src/driver/cxx-configfile-parser.c: $(srcdir)/src/driver/cxx-configfile-parser.y
if BISON_BUILDING
	$(BISON_verbose)$(BISON) -pconfigfile --debug --defines=src/driver/cxx-configfile-parser-internal.h --report=all --output=src/driver/cxx-configfile-parser.c $(srcdir)/src/driver/cxx-configfile-parser.y
else
	@echo "*** ERROR: file cxx-configfile-parser.y was modified but no suitable bison was found during configure ***"
	@exit 1
endif

src/driver/cxx-target-tools.h : Makefile
	$(AM_V_GEN)( \
		OUT_FILE=src/driver/cxx-target-tools.h; \
		echo "#ifndef CXX_TARGET_TOOLS_H" > $${OUT_FILE} ; \
		echo "#define CXX_TARGET_TOOLS_H" >> $${OUT_FILE} ; \
		echo "" >> $${OUT_FILE} ; \
		echo "#define TARGET_OBJDUMP \""$$(echo objdump | $(SED) '$(program_transform_name)')\" >> $${OUT_FILE} ; \
		echo "#define TARGET_OBJCOPY \""$$(echo objcopy | $(SED) '$(program_transform_name)')\" >> $${OUT_FILE} ; \
		echo "#define TARGET_LD \""$$(echo ld | $(SED) '$(program_transform_name)')\" >> $${OUT_FILE} ; \
		echo "#define TARGET_AR \""$$(echo ar | $(SED) '$(program_transform_name)')\" >> $${OUT_FILE} ; \
		echo "#define TARGET_MF03_PRESCANNER \""$$(echo mf03-prescanner | $(SED) '$(program_transform_name)')\" >> $${OUT_FILE} ; \
		echo "" >> $${OUT_FILE} ; \
		echo "#endif // CXX_TARGET_TOOLS_H" >> $${OUT_FILE}; \
		)

# Install hooks
driver-install-exec-hook:
	real_mcxx_name=`echo plaincxx | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
	for compiler_name in ${COMPILER_NAMES}; \
		do \
			real_compiler_name=`echo $${compiler_name} | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
			cd $(DESTDIR)/$(mcxxexecdir); $(LN_S) -f $${real_mcxx_name} $${real_compiler_name} ; \
		done

driver-uninstall-local:
	for compiler_name in @COMPILER_NAMES@; \
		do \
			real_compiler_name=`echo $${compiler_name} | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
			rm -f $(DESTDIR)/$(mcxxexecdir)/$${real_compiler_name} ; \
		done


# Embedder

src_driver_mcxx_embedder_CFLAGS= \
			-std=gnu99 \
			-I$(top_srcdir)/support/gperf \
			-I$(top_builddir)/support/gperf \
			-I$(top_srcdir)/lib \
			-I$(top_srcdir)/src/driver \
			-I$(top_srcdir)/src/driver/fortran \
			-I$(top_srcdir)/src/frontend \
			-I$(top_srcdir)/src/frontend/fortran \
			-I$(top_builddir)/lib \
			-I$(top_builddir)/src/driver \
			-I$(top_builddir)/src/driver/fortran \
			-I$(top_builddir)/src/frontend \
			-I$(top_builddir)/src/frontend/fortran \
			-DPKGDATADIR=\"$(pkgdatadir)\" \
			-Wall

src_driver_mcxx_embedder_SOURCES = \
                        src/driver/cxx-embedder.c \
                        src/driver/cxx-embed.c \
                        src/driver/cxx-embed.h \
                        src/driver/cxx-driver-utils.h \
                        src/driver/cxx-driver-utils.c \
                        src/driver/cxx-parameters.h \
                        src/driver/cxx-parameters.c \
                        $(END)

src_driver_mcxx_embedder_LDADD = $(src_driver_plaincxx_LDADD)
   $(END)

src_driver_mcxx_embedder_LDFLAGS = \
    -Wl,@RPATH_PARAMETER@,\$$ORIGIN/../lib \
	-Wl,--enable-new-dtags \
    $(END)

##########################################################################
# src/driver/fortran
##########################################################################

mcxxexec_PROGRAMS += src/driver/fortran/mf03-prescanner
noinst_LTLIBRARIES += src/driver/fortran/libmf03-driver.la

fortran_driver_common_cflags= \
			-std=gnu99 \
			-fexceptions \
			-I$(top_srcdir)/support/gperf \
			-I$(top_builddir)/support/gperf \
			-I$(top_srcdir)/lib \
			-I$(top_srcdir)/src/frontend \
			-I$(top_srcdir)/src/frontend/fortran \
			-I$(top_srcdir)/src/mcxx_tl \
			-I$(top_srcdir)/src/driver \
			-I$(top_srcdir)/src/driver/fortran \
			-I$(top_builddir)/lib \
			-I$(top_builddir)/src/frontend \
			-I$(top_builddir)/src/frontend/fortran \
			-I$(top_builddir)/src/mcxx_tl \
			-I$(top_builddir)/src/driver \
			-I$(top_builddir)/src/driver/fortran \
			-DPKGDATADIR=\"$(pkgdatadir)\" \
			-Wall

src_driver_fortran_mf03_prescanner_CFLAGS= $(fortran_driver_common_cflags)

src_driver_fortran_mf03_prescanner_SOURCES = \
                          src/driver/fortran/prescanner-driver.h \
                          src/driver/fortran/prescanner-driver.c \
                          src/driver/cxx-driver-utils.h \
                          src/driver/cxx-driver-utils.c \
                          $(END)

src_driver_fortran_mf03_prescanner_LDADD = $(src_driver_plaincxx_LDADD) \
   $(top_builddir)/src/frontend/fortran/libmf03-prescanner.la \
   $(END)

src_driver_fortran_libmf03_driver_la_SOURCES = \
  src/driver/fortran/cxx-driver-fortran.c \
  src/driver/fortran/cxx-driver-fortran.h \
  $(END)

src_driver_fortran_libmf03_driver_la_CFLAGS = $(fortran_driver_common_cflags)

##########################################################################
# config
##########################################################################

# We need a dist hook to keep these files in sync with their template.*
# see below
CONFIG_FILES =  \
       config/00.config.plain \
       config/01.config.intel-plain \
       config/02.config.utilities \
       config/10.config.hlt \
       config/10.config.omp-base \
       config/10.config.gomp-omp-base \
       config/10.config.intel-omp-base \
       config/40.config.cuda \
       config/50.config.omp.fpga \
       config/50.config.omp.mercurium \
       config/50.config.omp.profiling \
       config/50.config.omp.intel-rtl \
       config/50.config.omp.gomp-rtl \
       config/55.config.omp.cray   \
       config/55.config.omp.ibm    \
       config/55.config.omp.intel  \
       config/56.config.omp.mpi \
       config/56.config.omp.intel.mpi \
       config/57.config.omp.smp \
       config/57.config.omp.smp.intel \
       config/57.config.omp.smp.ibm \
	   $(END)

INSTALL_CONFIG_FILES = $(addprefix config/, @INSTALL_CONFIG_FILES@)

# We use this to remove existing config files that we know are deprecated and
# should not be in the user installation anymore
REMOVE_CONFIG_FILES=\
	10.config.analysis-base \
	50.config.cuda \
	50.config.gpu \
	50.config.omp.fortran \
	50.config.omp.ocl \
	55.config.omp.ibm.fortran \
	55.config.omp.intel.fortran \
	55.config.omp.nvidia \
	55.config.omp.ocl \
	56.config.omp.intel.fortran.mpi \
	57.config.omp.all \
	$(END)

config-files-dist-hook:
	$(MKDIR_P) $(distdir)/config
	for i in $(notdir $(CONFIG_FILES)); \
    do \
       cp -v "$(srcdir)/config/template.$${i}" "$(distdir)/config" || exit $?; \
    done


CLEANFILES += $(CONFIG_FILES)

# Replacements are defined in this file
config_edit = \
     $(SED) \
     -e 's|@NANOX_INCLUDES[@]|$(NANOX_INCLUDES)|g' \
     -e 's|@NANOX_LIBS[@]|$(NANOX_LIBS)|g' \
     -e 's|@NANOX_DYNAMIC_LIST[@]|$(NANOX_DYNAMIC_LIST)|g' \
     -e 's|@NANOX[@]|$(NANOX)|g' \
     -e 's|@NANOX_GNU_LDFLAGS[@]|$(NANOX_GNU_LDFLAGS)|g' \
     -e 's|@NANOX_IBM_LDFLAGS[@]|$(NANOX_IBM_LDFLAGS)|g' \
     -e 's|@NANOX_MIC_INCLUDES[@]|$(NANOX_MIC_INCLUDES)|g' \
     -e 's|@NANOX_MIC_LIBS[@]|$(NANOX_MIC_LIBS)|g' \
     -e 's|@NANOX_MIC[@]|$(NANOX_MIC)|g' \
     -e 's|@ENABLE_CUDA[@]|$(ENABLE_CUDA)|g' \
     -e 's|@ENABLE_OPENCL[@]|$(ENABLE_OPENCL)|g' \
     -e 's|@MPIFORT[@]|$(MPIFORT)|g' \
     -e 's|@MPICXX[@]|$(MPICXX)|g' \
     -e 's|@MPICC[@]|$(MPICC)|g' \
     -e "s|@abs_top_srcdir[@]|$(abs_top_srcdir)|g" \
     -e 's|@prefix[@]|$(prefix)|g' \
     -e 's|@includedir[@]|$(includedir)|g' \
     -e 's|@libdir[@]|$(libdir)|g' \
     -e 's|@RPATH_PARAMETER[@]|$(RPATH_PARAMETER)|g' \
     -e 's|@ICC[@]|$(ICC)|g' \
     -e 's|@ICPC[@]|$(ICPC)|g' \
     -e 's|@IFORT[@]|$(IFORT)|g' \
     -e 's|@XLC[@]|$(XLC)|g' \
     -e 's|@XLCXX[@]|$(XLCXX)|g' \
     -e 's|@XLF[@]|$(XLF)|g' \
     -e 's|@CRAYCC[@]|$(CRAYCC)|g' \
     -e 's|@CRAYCXX[@]|$(CRAYCXX)|g' \
     -e 's|@CRAYFC[@]|$(CRAYFC)|g' \
     -e 's|@NVCC[@]|$(NVCC)|g' \
     -e 's|@PKGDATADIR[@]|$(pkgdatadir)|g' \
     -e 's|@SIMD_FLAGS[@]|$(SIMD_FLAGS)|g' \
     -e 's|@SIMD_INCLUDES[@]|$(SIMD_INCLUDES)|g' \
     -e 's|@MPICC[@]|$(MPICC)|g' \
     -e 's|@MPICXX[@]|$(MPICXX)|g' \
     -e 's|@MIC_LIBS[@]|$(MIC_LIBS)|g' \
     -e 's|@INTEL_OMP_LIB[@]|$(INTEL_OMP_LIB)|g' \
     -e 's|@INTEL_OMP_INCLUDE[@]|$(INTEL_OMP_INCLUDE)|g' \
     -e 's|@MIC_TOOLS[@]|$(MIC_TOOLS)|g' \
     -e 's|@CORRECTNESS_LOG_DIR[@]|$(CORRECTNESS_LOG_DIR)|g' \
     -e 's|@NANOX_GATE[@]|$(NANOX_GATE)|g' \
     -e 's|@NANOS6_GATE[@]|$(NANOS6_GATE)|g' \
     -e 's|@NANOS6_INCLUDES[@]|$(NANOS6_INCLUDES)|g' \
     -e 's|@NANOS6_LIB[@]|$(NANOS6_LIB)|g' \
     -e 's|@GOMP_OMP_LIB[@]|$(GOMP_OMP_LIB)|g' \
     -e 's|@GFORTRAN_8_OR_GREATER_ARRAY_DESCRIPTORS[@]|$(GFORTRAN_8_OR_GREATER_ARRAY_DESCRIPTORS)|g' \
     -e 's|@TCL_GATE[@]|$(TCL_GATE)|g' \
     -e 's|@TCL_INCLUDES[@]|$(TCL_INCLUDES)|g' \
     -e 's|@TCL_LIB[@]|$(TCL_LIB)|g' \
    $(END)

# Cross compiler profile names
#
# Profiles of Mercurium
profiles = mcxx $(COMPILER_NAMES) $(SCRIPT_COMPILER_NAMES)
config_edit += $(foreach profile, $(profiles), \
    -e 's|^\[\( *\)$(profile)\( *\)\(:.*\)\?\]|\[\1'`echo '$(profile)' | $(SED) '$(program_transform_name)'`'\2\3\]|g' \
    -e 's|^\[\(.*\):\( *\)$(profile)\( *\)\]|\[\1:\2'`echo '$(profile)' | $(SED) '$(program_transform_name)'`'\3\]|g' \
    )

# gcc, g++ and gfortran
config_edit += -e 's|@GCC[@]|'`echo $(GCC) | $(SED) '$(program_transform_name)'`'|g' \
	-e 's|@G++[@]|'`echo $(GXX) | $(SED) '$(program_transform_name)'`'|g' \
	-e 's|@GFORTRAN[@]|'`echo $(GFORTRAN) | $(SED) '$(program_transform_name)'`'|g'

# target_options[profile]
config_edit += $(foreach profile, $(profiles), \
  -e 's|^\( *\)*target_options\[\( *\)$(profile)\( *\)\]\( .*\)=|\1target_options[\2'`echo '$(profile)' | $(SED) '$(program_transform_name)'`'\3]\4=|g' \
   )

BUILT_SOURCES += $(INSTALL_CONFIG_FILES) $(INSTALL_SCRIPT_COMPILER_NAMES)

# Generates configs
config/% : config/template.$(notdir %) Makefile
	$(AM_V_GEN)( $(config_edit) "$<" > "$@.tmp"; mv -f "$@.tmp" "$@" )
 
config-install-data-hook: remove-old-profiles

EXTRA_DIST += \
       config/template.mpimcc \
       config/template.mpimcxx \
       config/template.mpimfc \
	   $(END)

# Generates driver scripts
config/driver-scripts:
	$(MKDIR_P) config/driver-scripts
config/driver-scripts/%: config/template.$(notdir %) Makefile | config/driver-scripts
	$(AM_V_GEN)( $(config_edit) "$<" > "$@.tmp"; mv -f "$@.tmp" "$@" )

INSTALL_SCRIPT_COMPILER_NAMES=$(addprefix config/driver-scripts/, @SCRIPT_COMPILER_NAMES@)

mcxxexec_SCRIPTS =
mcxxexec_SCRIPTS += $(INSTALL_SCRIPT_COMPILER_NAMES)

script-driver-install-data-hook:

remove-old-profiles:
	for oldfile in $(REMOVE_CONFIG_FILES); \
	do \
		if test -e $(configdir)/$${oldfile};\
		then \
			echo "Removing old configuration file $${oldfile}"; \
			rm -f $(configdir)/$${oldfile}; \
		fi \
	done

install-data-hook: config-install-data-hook script-driver-install-data-hook

configdir = $(pkgdatadir)/config.d
config_DATA = \
        $(INSTALL_CONFIG_FILES)

##########################################################################
# scripts
##########################################################################

EXTRA_DIST += \
			scripts/bison-2.3-rofi-patch \
			scripts/mcxx-rpm.spec \
			scripts/simd/x86/builtins_ia32.cpp \
			scripts/simd/neon/builtins_neon.cpp \
			scripts/simd/builtins-common.hpp \
			$(DEBIAN_EXTRA) \
			$(LIT_EXTRA) \
			$(END)

DEBIAN_EXTRA=\
              scripts/debian/compat \
              scripts/debian/source/format \
              scripts/debian/rules \
              scripts/debian/control \
              $(END)

LIT_EXTRA=\
          scripts/lit \
          scripts/lit/lit.py \
          scripts/lit/setup.cfg \
          scripts/lit/PKG-INFO \
          scripts/lit/setup.py \
          scripts/lit/MANIFEST.in \
          scripts/lit/lit \
          scripts/lit/lit/main.py \
          scripts/lit/lit/__init__.py \
          scripts/lit/lit/LitTestCase.py \
          scripts/lit/lit/ShCommands.py \
          scripts/lit/lit/BooleanExpression.py \
          scripts/lit/lit/ProgressBar.py \
          scripts/lit/lit/discovery.py \
          scripts/lit/lit/run.py \
          scripts/lit/lit/builtin_commands \
          scripts/lit/lit/builtin_commands/cat.py \
          scripts/lit/lit/builtin_commands/__init__.py \
          scripts/lit/lit/LitConfig.py \
          scripts/lit/lit/llvm \
          scripts/lit/lit/llvm/subst.py \
          scripts/lit/lit/llvm/__init__.py \
          scripts/lit/lit/llvm/config.py \
          scripts/lit/lit/TestingConfig.py \
          scripts/lit/lit/TestRunner.py \
          scripts/lit/lit/ShUtil.py \
          scripts/lit/lit/util.py \
          scripts/lit/lit/Test.py \
          scripts/lit/lit/formats \
          scripts/lit/lit/formats/__init__.py \
          scripts/lit/lit/formats/googletest.py \
          scripts/lit/lit/formats/base.py \
          scripts/lit/lit/formats/shtest.py \
          scripts/lit/README.txt \
		  $(END)

#####################################
# x86 builtins generator for GCC
#####################################

GXX_X86_BUILTINS = g++-7
GXX_X86_DEPRECATED_BUILTINS = g++-5

# Common flags between GXX_X86_BUILTINS and GXX_X86_DEPRECATED_BUILTINS
GXX_X86_COMMON_FLAGS =
GXX_X86_COMMON_FLAGS += -fabi-version=6
GXX_X86_COMMON_FLAGS += -m3dnow
GXX_X86_COMMON_FLAGS += -mabm
GXX_X86_COMMON_FLAGS += -maes
GXX_X86_COMMON_FLAGS += -mavx
GXX_X86_COMMON_FLAGS += -mavx2
GXX_X86_COMMON_FLAGS += -mavx512bw
GXX_X86_COMMON_FLAGS += -mavx512cd
GXX_X86_COMMON_FLAGS += -mavx512dq
GXX_X86_COMMON_FLAGS += -mavx512er
GXX_X86_COMMON_FLAGS += -mavx512f
GXX_X86_COMMON_FLAGS += -mavx512ifma
GXX_X86_COMMON_FLAGS += -mavx512pf
GXX_X86_COMMON_FLAGS += -mavx512vbmi
GXX_X86_COMMON_FLAGS += -mavx512vl
GXX_X86_COMMON_FLAGS += -mbmi
GXX_X86_COMMON_FLAGS += -mbmi2
GXX_X86_COMMON_FLAGS += -mclflushopt
GXX_X86_COMMON_FLAGS += -mclwb
GXX_X86_COMMON_FLAGS += -mf16c
GXX_X86_COMMON_FLAGS += -mfma
GXX_X86_COMMON_FLAGS += -mfma4
GXX_X86_COMMON_FLAGS += -mfsgsbase
GXX_X86_COMMON_FLAGS += -mfxsr
GXX_X86_COMMON_FLAGS += -mlwp
GXX_X86_COMMON_FLAGS += -mlzcnt
GXX_X86_COMMON_FLAGS += -mmmx
GXX_X86_COMMON_FLAGS += -mmpx
GXX_X86_COMMON_FLAGS += -mmwaitx
GXX_X86_COMMON_FLAGS += -mno-fma4
GXX_X86_COMMON_FLAGS += -mpclmul
GXX_X86_COMMON_FLAGS += -mpopcnt
GXX_X86_COMMON_FLAGS += -mprefetchwt1
GXX_X86_COMMON_FLAGS += -mrdrnd
GXX_X86_COMMON_FLAGS += -mrdseed
GXX_X86_COMMON_FLAGS += -mrtm
GXX_X86_COMMON_FLAGS += -msha
GXX_X86_COMMON_FLAGS += -msse
GXX_X86_COMMON_FLAGS += -msse2
GXX_X86_COMMON_FLAGS += -msse3
GXX_X86_COMMON_FLAGS += -msse4
GXX_X86_COMMON_FLAGS += -msse4.1
GXX_X86_COMMON_FLAGS += -msse4.2
GXX_X86_COMMON_FLAGS += -msse4a
GXX_X86_COMMON_FLAGS += -mssse3
GXX_X86_COMMON_FLAGS += -mtbm
GXX_X86_COMMON_FLAGS += -mxop
GXX_X86_COMMON_FLAGS += -mxsave
GXX_X86_COMMON_FLAGS += -mxsavec
GXX_X86_COMMON_FLAGS += -mxsaveopt
GXX_X86_COMMON_FLAGS += -mxsaves
GXX_X86_COMMON_FLAGS += -mclzero
GXX_X86_COMMON_FLAGS += -mpku

GXX_X86_BUILTINS_FLAGS = $(GXX_X86_COMMON_FLAGS)
GXX_X86_BUILTINS_FLAGS += -mavx5124fmaps     # introduced in GCC-7
GXX_X86_BUILTINS_FLAGS += -mavx5124vnniw     # introduced in GCC-7
GXX_X86_BUILTINS_FLAGS += -mavx512vpopcntdq  # introduced in GCC-7
GXX_X86_BUILTINS_FLAGS += -mrdpid            # introduced in GCC-7

GXX_X86_DEPRECATED_BUILTINS_FLAGS = $(GXX_X86_COMMON_FLAGS)
GXX_X86_DEPRECATED_BUILTINS_FLAGS += -mpcommit # deprecated in GCC-6

scripts/generate_builtins_ia32.bin : $(srcdir)/scripts/simd/x86/builtins_ia32.cpp Makefile
	$(GXX_X86_BUILTINS) $(GXX_X86_BUILTINS_FLAGS) -I$(srcdir)/scripts/simd \
		-o scripts/generate_builtins_ia32.bin \
		$(srcdir)/scripts/simd/x86/builtins_ia32.cpp \
		-Wl,-rpath,$$(dirname $$($(GXX_X86_BUILTINS) -print-file-name=libstdc++.so))

# These files are only regenerated manually
.PHONY: generate_builtins_ia32
CLEANFILES += scripts/generate_builtins_ia32.bin
generate_builtins_ia32: scripts/generate_builtins_ia32.bin
	./scripts/generate_builtins_ia32.bin > $(top_srcdir)/src/frontend/cxx-gccbuiltins-ia32.h

scripts/generate_builtins_ia32_deprecated.bin : $(srcdir)/scripts/simd/x86/builtins_ia32_deprecated.cpp Makefile
	$(GXX_X86_DEPRECATED_BUILTINS) $(GXX_X86_DEPRECATED_BUILTINS_FLAGS) -I$(srcdir)/scripts/simd \
		-o scripts/generate_builtins_ia32_deprecated.bin \
		$(srcdir)/scripts/simd/x86/builtins_ia32_deprecated.cpp \
		-Wl,-rpath,$$(dirname $$($(GXX_X86_DEPRECATED_BUILTINS) -print-file-name=libstdc++.so))

# These files are only regenerated manually
.PHONY: generate_builtins_ia32_deprecated
CLEANFILES += scripts/generate_builtins_ia32.bin
generate_builtins_ia32_deprecated: scripts/generate_builtins_ia32_deprecated.bin
	./scripts/generate_builtins_ia32_deprecated.bin > $(top_srcdir)/src/frontend/cxx-gccbuiltins-ia32-deprecated.h

#####################################
# x86 builtins generator for ICC
#####################################

scripts/generate_builtins_icc.bin : $(srcdir)/scripts/simd/x86/builtins_icc.cpp scripts/simd/builtins-common.hpp scripts/simd/builtins-common-icc.hpp Makefile
	icpc -std=c++11 -I$(srcdir)/scripts/simd \
		-O0 -o scripts/generate_builtins_icc.bin \
		$(srcdir)/scripts/simd/x86/builtins_icc.cpp

# These files are only regenerated manually
.PHONY: generate_builtins_icc
CLEANFILES += scripts/generate_builtins_icc.bin
generate_builtins_icc: scripts/generate_builtins_icc.bin
	./scripts/generate_builtins_icc.bin > $(top_srcdir)/src/frontend/cxx-iccbuiltins.h

#####################################
# Knights Corner
#####################################

scripts/generate_builtins_icc_knc.bin : $(srcdir)/scripts/simd/x86/builtins_icc_knc.cpp scripts/simd/builtins-common.hpp scripts/simd/builtins-common-icc.hpp Makefile
	icpc -std=c++11 -mmic -I$(srcdir)/scripts/simd \
		-O0 -o scripts/generate_builtins_icc_knc.bin \
		$(srcdir)/scripts/simd/x86/builtins_icc_knc.cpp

# These files are only regenerated manually
.PHONY: generate_builtins_icc_knc
CLEANFILES += scripts/generate_builtins_icc_knc.bin
generate_builtins_icc_knc: scripts/generate_builtins_icc_knc.bin
	micnativeloadex ./scripts/generate_builtins_icc_knc.bin > $(top_srcdir)/src/frontend/cxx-iccbuiltins-knc.h

#####################################
# ARM NEON builtins generator
#####################################

LATEST_GXX_NEON=arm-linux-gnueabihf-g++
LATEST_GXX_NEON_FLAGS =
LATEST_GXX_NEON_FLAGS += -mfpu=neon

# These files are only regenerated manually
.PHONY: generate_builtins_neon
CLEANFILES += scripts/generate_builtins_neon.bin

scripts/generate_builtins_neon.bin : $(srcdir)/scripts/simd/neon/builtins_neon.cpp Makefile
	$(LATEST_GXX_NEON) $(LATEST_GXX_NEON_FLAGS) \
		-static \
		-I$(srcdir)/scripts/simd \
		-o scripts/generate_builtins_neon.bin \
		$(srcdir)/scripts/simd/neon/builtins_neon.cpp

generate_builtins_neon: scripts/generate_builtins_neon.bin
	qemu-arm ./scripts/generate_builtins_neon.bin > $(top_srcdir)/src/frontend/cxx-gccbuiltins-arm-neon.h

#####################################
# ARM64 NEON builtins generator
#####################################

LATEST_GXX_NEON_ARM64=/apps/gcc/7.2.0/bin/aarch64-unknown-linux-gnu-g++
LATEST_GXX_NEON_ARM64_FLAGS =

# These files are only regenerated manually
.PHONY: generate_builtins_neon_arm64
CLEANFILES += scripts/generate_builtins_neon_arm64.bin

scripts/generate_builtins_neon_arm64.bin : $(srcdir)/scripts/simd/neon/builtins_neon_arm64.cpp Makefile
	$(LATEST_GXX_NEON_ARM64) $(LATEST_GXX_NEON_ARM64_FLAGS) \
		-static \
		-I$(srcdir)/scripts/simd \
		-o scripts/generate_builtins_neon_arm64.bin \
		$(srcdir)/scripts/simd/neon/builtins_neon_arm64.cpp

generate_builtins_neon_arm64: scripts/generate_builtins_neon_arm64.bin
	./scripts/generate_builtins_neon_arm64.bin > $(top_srcdir)/src/frontend/cxx-gccbuiltins-arm64-neon.h

#####################################

rpm: dist-gzip
	@echo "Generating RPM structure"
	cd scripts; \
	$(MKDIR_P) -p SOURCES BUILD RPMS SPECS SRPMS; \
	cp ../$(distdir).tar.gz SOURCES; \
	THREADS=$$(getconf _NPROCESSORS_ONLN); THREADS=$${THREADS:-1};\
	rpmbuild -v -bb --clean $(abs_top_srcdir)/scripts/mcxx-rpm.spec --define "_topdir $$(pwd)" --define '_version $(VERSION)' --define '_release $(shell date +%Y%m%d)' --define '_rpm_prefix $(RPM_PREFIX)' --define "threads $${THREADS}";

scripts-clean-rpm:
	rm -rf scripts/BUILDROOT scripts/SOURCES scripts/BUILD scripts/RPMS scripts/SPECS scripts/SRPMS

deb: dist-gzip
	cd scripts; \
	cp ../$(distdir).tar.gz mcxx_$(VERSION).orig.tar.gz; \
	tar -xf mcxx_$(VERSION).orig.tar.gz; \
	rm -rf upstream_dir; \
	mv mcxx-$(VERSION) upstream_dir; \
	cp -rf $(abs_top_srcdir)/scripts/debian upstream_dir/; \
	cp -rf upstream_dir/COPYING upstream_dir/debian/copyright; \
	rm -f upstream_dir/debian/changelog; \
	if test ! -d $(abs_top_srcdir)/.git; \
	then dch --create -c upstream_dir/debian/changelog --empty --package mcxx -v "$(VERSION)-$(shell date +%Y%m%d)$(DEB_RELEASE)"; \
	else git --git-dir="$(abs_top_srcdir)/.git" --work-tree="$(abs_top_srcdir)/.git" log -1 --pretty=format:"mcxx ($(VERSION)-$(shell date +%Y%m%d)$(DEB_RELEASE)) unstable; urgency=low%x0A%x0A  * %h %s%x0A%x0A -- %an <%ae>  %aD" > upstream_dir/debian/changelog; \
	fi; \
	THREADS=$$(getconf _NPROCESSORS_ONLN); THREADS=$${THREADS:-1}; \
	cd upstream_dir; DEB_BUILD_OPTIONS="nocheck parallel=$${THREADS}" debuild -us -uc; \
	rm -rf upstream_dir ;

scripts-clean-deb:
	cd scripts; \
	rm -f mcxx_$(VERSION).orig.tar.gz mcxx_$(VERSION)*.debian.tar.gz mcxx_$(VERSION)*.dsc mcxx_$(DEB_RELEASE)*.build mcxx_$(DEB_RELEASE)*.changes *.deb; \
	rm -rf upstream_dir

scripts-clean-local: scripts-clean-rpm scripts-clean-deb

##########################################################################
# support
##########################################################################

# Empty
#
# Usually changes in these files should cause a rebuild of the modules

FORTRAN_FRONTEND_FILES=\
					   $(top_builddir)/src/driver/plaincxx \
					   $(top_srcdir)/src/frontend/fortran/fortran03-modules.c \
					   $(top_srcdir)/src/frontend/fortran/fortran03-modules.h \
					   $(END)

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

PLAINFC_PROFILE=`echo plainfc | $(SED) '$(program_transform_name)'`

PLAINFC=$(top_builddir)/src/driver/plaincxx --config-dir=$(top_builddir)/config --profile=$(PLAINFC_PROFILE) --do-not-warn-config --do-not-wrap-modules

##########################################################################
# support/gperf
##########################################################################

if GPERF_BUILDING
BUILT_SOURCES += support/gperf/gperf-compat-types.h
CLEANFILES += support/gperf/gperf-compat-types.h
endif

EXTRA_DIST += support/gperf/gperf-compat-types.h

if GPERF_BUILDING
GPERF_LEN_TYPE=@GPERF_LENGTH_TYPE@
support/gperf/gperf-compat-types.h: Makefile
	$(AM_V_GEN)( \
    OUT_FILE="support/gperf/gperf-compat-types.h"; \
	echo "/* This file has been generated */" > $${OUT_FILE}; \
	echo >> $${OUT_FILE}; \
	echo "#ifndef GPERF_COMPAT_TYPES_H" >> $${OUT_FILE}; \
	echo "#define GPERF_COMPAT_TYPES_H" >> $${OUT_FILE}; \
	echo >> $${OUT_FILE}; \
	echo "#include<stddef.h>" >> $${OUT_FILE}; \
	echo "typedef" $(GPERF_LEN_TYPE) "gperf_length_t;" >> $${OUT_FILE}; \
	echo >> $${OUT_FILE}; \
	echo "#endif /* GPERF_COMPAT_TYPES_H */" >> $${OUT_FILE}; \
    )
endif

##########################################################################
# support/iso_fortran
##########################################################################

fortran_modulesdir=$(pkgdatadir)/fortran
fortran_modules_DATA = \
			  support/iso_fortran/iso_fortran_env.mf03

EXTRA_DIST += \
			support/iso_fortran/iso_c_binding.f90 \
			support/iso_fortran/iso_fortran_env.f90 \
			$(END)

CLEANFILES += support/iso_fortran/iso_fortran_env.mf03

support/iso_fortran/iso_fortran_env.mf03 : $(srcdir)/support/iso_fortran/iso_fortran_env.f90 $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/iso_fortran $(srcdir)/support/iso_fortran/iso_fortran_env.f90

##########################################################################
# support/openmp
##########################################################################

# Empty

##########################################################################
# support/openmp/fortran
##########################################################################

omp_modules = \
    support/openmp/fortran/omp_lib_kinds.mf03 \
    support/openmp/fortran/omp_lib.mf03 \
    $(END)

EXTRA_DIST += \
		   support/openmp/fortran/omp_modules.f90 \
		   support/openmp/fortran/omp_lib.h \
		   $(END)

CLEANFILES += $(omp_modules)

fortran_modules_ompdir=$(pkgdatadir)/fortran/openmp
fortran_modules_omp_DATA = \
			  $(omp_modules) \
			  support/openmp/fortran/omp_lib.h \
			  $(END)

# This is a noop to avoid breaking parallel builds
support/openmp/fortran/omp_lib_kinds.mf03 : support/openmp/fortran/omp_lib.mf03

support/openmp/fortran/omp_lib.mf03 : $(srcdir)/support/openmp/fortran/omp_modules.f90 $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/openmp/fortran $(srcdir)/support/openmp/fortran/omp_modules.f90

##########################################################################
# support/gomp_omp
##########################################################################

if BUILD_OMP_GOMP

gomp_openmpdir=$(pkgdatadir)/gomp-omp

gomp_openmp_DATA = \
			  support/gomp_omp/gomp-omp.h \
			  $(END)

endif

EXTRA_DIST += \
			support/gomp_omp/gomp-omp.h \
			$(END)

##########################################################################
# support/intel_omp
##########################################################################

if BUILD_OMP_INTEL

intel_openmpdir=$(pkgdatadir)/intel-omp

intel_openmp_DATA = \
			  support/intel_omp/intel-omp.h \
			  support/intel_omp/intel-omp-atomics.h \
			  $(END)

endif

EXTRA_DIST += \
			support/intel_omp/atomic-funs.py \
			support/intel_omp/atomic-list.def \
			support/intel_omp/intel-omp-atomics.h \
			support/intel_omp/intel-omp.h \
			$(END)

##########################################################################
# support/mpi
##########################################################################

# Empty

##########################################################################
# support/mpi/mpich
##########################################################################

mpich_modules = \
			  support/mpi/mpich/mpi.mf03 \
			  support/mpi/mpich/mpi_constants.mf03 \
			  support/mpi/mpich/mpi_base.mf03 \
			  support/mpi/mpich/mpi_sizeofs.mf03 \
			  $(END)

EXTRA_DIST += \
    support/mpi/mpich/mpi_base.f90 \
    support/mpi/mpich/mpi_constants.f90 \
    support/mpi/mpich/mpi.f90 \
    support/mpi/mpich/mpifnoext.h \
    support/mpi/mpich/mpi_sizeofs.f90 \
    $(END)

CLEANFILES += $(mpich_modules) support/mpi/mpich/mpi_base.mf03 support/mpi/mpich/mpi_constants.mf03 support/mpi/mpich/mpi_sizeofs.mf03

fortran_modules_mpichdir=$(pkgdatadir)/fortran/mpi/mpich
fortran_modules_mpich_DATA = \
			  $(mpich_modules) \
			  $(END)

support/mpi/mpich/mpi.mf03 : support/mpi/mpich/mpi_constants.mf03 support/mpi/mpich/mpi_sizeofs.mf03 support/mpi/mpich/mpi_base.mf03 $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/mpi/mpich  $(srcdir)/support/mpi/mpich/mpi.f90

support/mpi/mpich/mpi_constants.mf03 : $(srcdir)/support/mpi/mpich/mpi_constants.f90 $(srcdir)/support/mpi/mpich/mpifnoext.h $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/mpi/mpich $(srcdir)/support/mpi/mpich/mpi_constants.f90

support/mpi/mpich/mpi_sizeofs.mf03 : $(srcdir)/support/mpi/mpich/mpi_sizeofs.f90 $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/mpi/mpich $(srcdir)/support/mpi/mpich/mpi_sizeofs.f90

support/mpi/mpich/mpi_base.mf03 : support/mpi/mpich/mpi_constants.mf03 $(srcdir)/support/mpi/mpich/mpi_base.f90 $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/mpi/mpich $(srcdir)/support/mpi/mpich/mpi_base.f90

##########################################################################
# support/mpi/openmpi
##########################################################################

openmpi_modules = \
			  support/mpi/openmpi/mpi.mf03 \
			  $(END)

EXTRA_DIST += \
    support/mpi/openmpi/attr_fn-f90-interfaces.h \
    support/mpi/openmpi/conversion_fn_null-f90-interface.h \
    support/mpi/openmpi/mpi.F90 \
    support/mpi/openmpi/mpi-f90-interfaces.h \
    support/mpi/openmpi/mpif-common.h \
    support/mpi/openmpi/mpif-config.h \
    support/mpi/openmpi/mpif-mpi-io.h \
    $(END)

CLEANFILES += $(openmpi_modules)

fortran_modules_openmpidir=$(pkgdatadir)/fortran/mpi/openmpi
fortran_modules_openmpi_DATA = \
			  $(openmpi_modules) \
			  $(END)

support/mpi/openmpi/mpi.mf03 : $(srcdir)/support/mpi/openmpi/attr_fn-f90-interfaces.h \
	      $(srcdir)/support/mpi/openmpi/conversion_fn_null-f90-interface.h \
		  $(srcdir)/support/mpi/openmpi/mpi.F90 \
		  $(srcdir)/support/mpi/openmpi/mpi-f90-interfaces.h \
		  $(srcdir)/support/mpi/openmpi/mpif-common.h \
		  $(srcdir)/support/mpi/openmpi/mpif-config.h \
		  $(srcdir)/support/mpi/openmpi/mpif-mpi-io.h \
		  $(FORTRAN_FRONTEND_FILES)
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null -J support/mpi/openmpi $(srcdir)/support/mpi/openmpi/mpi.F90

##########################################################################
# support/romol
##########################################################################

if BUILD_VECTORIZATION

romoldir=$(pkgdatadir)/romol

romol_DATA = \
			  support/romol/valib.h \
			  $(END)

endif

EXTRA_DIST += \
			support/romol/valib.h \
			$(END)

##########################################################################
# Hooks
##########################################################################

install-exec-hook : driver-install-exec-hook

uninstall-local : driver-uninstall-local

clean-local: scripts-clean-rpm \
	         scripts-clean-deb

dist-hook : config-files-dist-hook git-version-dist-hook

# This is a hack that workarounds libtool trying to relink in parallel when it
# should wait for all its dependences. This only happens when a single Makefile
# install different LTLIBRARIES into different directories
#
# See: http://lists.gnu.org/archive/html/libtool/2011-03/msg00004.html
#
# The hack actually uses a variable called 'install_phasesLTLIBRARIES' so
# Automake does not see that we are actually overriding the rule. If Automake
# noticed that we are overriding the rule, it would not emit the rule at all:
# this is not what we want because we need to keep all the usual libtool
# wizardry
install_phasesLTLIBRARIES = install-phasesLTLIBRARIES
$(install_phasesLTLIBRARIES): install-libLTLIBRARIES
# The hack ends here
