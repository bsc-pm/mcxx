##########################################################################
#  (C) Copyright 2006-2011 Barcelona Supercomputing Center               #
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

lib_LTLIBRARIES = libmf03.la libmf03-prescanner.la

END=
EXTRA_DIST=
CLEANFILES=
BUILT_SOURCES=

BUILT_SOURCES += fortran03-scanner.c
BUILT_SOURCES += fortran03-parser.c
BUILT_SOURCES += fortran03-parser-internal.h
BUILT_SOURCES += fortran03.y
BUILT_SOURCES += prescanner-scanner.h

# Mercurium C/C++ compiler runtime library
libmf03_la_SOURCES = \
                     fortran03-parser.c \
                     fortran03-parser.h \
                     fortran03-scanner.c \
                     fortran03-parser-internal.h \
                     fortran03-utils.h \
                     fortran03-utils.c \
                     fortran03-prettyprint.c \
                     fortran03-prettyprint.h \
                     fortran03-split.h \
                     fortran03-split.c \
                     fortran03-scope-decls.h \
                     fortran03-scope.h \
                     fortran03-scope.c \
                     fortran03-buildscope.h \
                     fortran03-buildscope.c \
                     fortran03-exprtype.h \
                     fortran03-exprtype.c \
                     libmf03-common.h \
                     $(END)

libmf03_la_LIBADD = \
					$(top_builddir)/lib/libmcxx-utils.la \
					$(top_builddir)/lib/libmcxx-extstruct.la \
                    $(top_builddir)/src/frontend/libmcxx-process.la \
					$(END)

libmf03_la_LDFLAGS = $(dll_exports) -avoid-version $(no_undefined)

libmf03_la_CFLAGS = -DLIBMF03_DLL_EXPORT \
                    -Wall \
					-Wshadow \
					-Wextra \
				    -I$(top_srcdir)/lib \
				    -I$(top_srcdir)/src/frontend \
				    -I$(top_builddir)/src/frontend \
				    -I$(top_srcdir)/src/driver \
                    -fexceptions \
				    -DPKGDATADIR=\"$(pkgdatadir)\"

# Prescanner
libmf03_prescanner_la_SOURCES = \
                                prescanner-identifier.c \
                                prescanner-identifier.h \
                                prescanner-process.c \
                                prescanner-process-decls.h \
                                prescanner-process.h \
                                libmf03-prescanner-common.h \
                                $(END)

libmf03_prescanner_la_CFLAGS = -DLIBMF03_PRESCANNER_DLL_EXPORT \
                    -Wall \
					-Wshadow \
					-Wextra \
				    -I$(top_srcdir)/lib \
				    -I$(top_srcdir)/src/frontend \
				    -I$(top_builddir)/src/frontend \
				    -I$(top_srcdir)/src/driver \
                    -fexceptions \
				    -DPKGDATADIR=\"$(pkgdatadir)\"

# Remaining stuff

ADDITIONAL_GRAMMARS = ../cxx-pragma.y

TPP=$(top_builddir)/lib/tpp$(EXEEXT)

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
endif

EXTRA_DIST += fortran03.y
EXTRA_DIST += fortran03.y.in

EXTRA_DIST += prescanner.l

fortran03.y : $(srcdir)/fortran03.y.in $(addprefix $(srcdir)/, $(ADDITIONAL_GRAMMARS)) 
	$(TPP_verbose)(rm -f fortran03.y && $(TPP) -I $(srcdir)/.. -o fortran03.y -D FORTRAN2003 -I$(srcdir) $(srcdir)/fortran03.y.in && chmod -w fortran03.y)

if BISON_BUILDING
CLEANFILES += fortran03-parser.c
CLEANFILES += fortran03-parser-internal.h
CLEANFILES += fortran03-parser.output
CLEANFILES += fortran03.y
endif

fortran03-parser-internal.h : fortran03-parser.c

EXTRA_DIST += fortran03-parser.c

fortran03-parser.c : fortran03.y
if BISON_BUILDING
	$(BISON_verbose)$(BISON) -pmf03 --debug --defines=fortran03-parser-internal.h --report=all --output=fortran03-parser.c fortran03.y
else
	@echo "*** ERROR: file fortran03.y was modified but no suitable bison was found during configure ***"
	@exit 1
endif

EXTRA_DIST += fortran03.l

if FLEX_BUILDING
CLEANFILES += fortran03-scanner.c
endif

EXTRA_DIST += fortran03-scanner.c

fortran03-scanner.c : fortran03.l fortran03-parser-internal.h
if FLEX_BUILDING
	$(FLEX_verbose)$(FLEX) -Pmf03 -d -i -ofortran03-scanner.c -8 $(srcdir)/fortran03.l
else
	@echo "*** ERROR: file fortran03.l was modified but no suitable flex was found during configure ***"
	@exit 1
endif

CLEANFILES += prescanner-scanner.h

prescanner-scanner.h : $(srcdir)/prescanner-identifier.h $(srcdir)/prescanner.l
if FLEX_BUILDING
	$(FLEX_verbose)$(FLEX) -d -i -oprescanner-scanner.h -8 $(srcdir)/prescanner.l
else
	@echo "*** ERROR: file prescanner.l was modified but no suitable flex was found during configure ***"
	@exit 1
endif

