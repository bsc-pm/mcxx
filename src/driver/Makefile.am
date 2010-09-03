##########################################################################
#  (C) Copyright 2006-2009 Barcelona Supercomputing Center               #
#                          Centro Nacional de Supercomputacion           #
#                                                                        #
#  This file is part of Mercurium C/C++ source-to-source compiler.       #
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

# mcxxexec can't be changed to simply 'bin' or 'mcxx' (otherwise things won't
# work)
mcxxexec_PROGRAMS = mcxx mcxx-embedder

if ENABLE_PKGLIB_INSTALL
mcxxexecdir = $(pkglibdir)
else
mcxxexecdir = @bindir@
endif

EXTRA_DIST =
CLEANFILES = 
BUILT_SOURCES =

END=

mcxx_CFLAGS=-fexceptions \
			-I$(top_srcdir)/lib \
			-I$(top_srcdir)/src/frontend \
			-I$(top_srcdir)/src/mcxx_tl \
			-I$(top_builddir)/lib \
			-I$(top_builddir)/src/frontend \
			-I$(top_builddir)/src/mcxx_tl \
			-DPKGDATADIR=\"$(pkgdatadir)\" \
			-Wall

mcxx_SOURCES = \
  cxx-configfile.h \
  cxx-configfile.c \
  cxx-configoptions.c \
  cxx-parameters.c \
  cxx-parameters.h \
  cxx-fileextensions.c \
  cxx-debugflags.c \
  cxx-driver.c \
  cxx-driver-decls.h \
  cxx-driver.h \
  cxx-driver-utils.c \
  cxx-driver-utils.h \
  cxx-profile.c \
  cxx-profile.h \
  cxx-configfile-parser-internal.h \
  cxx-configfile-parser.h \
  cxx-configfile-parser.c \
  cxx-configfile-lexer.h \
  cxx-configfile-lexer.c \
  cxx-multifile.h \
  cxx-multifile.c \
  cxx-embed.c \
  cxx-embed.h \
  $(END)

mcxx_LDADD = \
   $(top_builddir)/gnulib/libgnulib.la \
   $(top_builddir)/lib/libmcxx-utils.la \
   $(top_builddir)/lib/libmcxx-extstruct.la \
   $(top_builddir)/src/frontend/libmcxx-process.la \
   $(top_builddir)/src/frontend/libmcxx.la \
   $(top_builddir)/src/tl/libtl.la \
   $(top_builddir)/src/mcxx_tl/libmcxxtl.la \
   $(END)

if WINDOWS_BUILD
else
# '$$ORIGIN' is a relocatable hint to GNU ld
mcxx_LDFLAGS = \
    -Wl,-E \
    -Wl,@RPATH_PARAMETER@,$(libdir) \
    -Wl,@RPATH_PARAMETER@,$(pkglibdir) \
    -Wl,@RPATH_PARAMETER@,'$$ORIGIN'/../lib \
    -Wl,@RPATH_PARAMETER@,'$$ORIGIN'/../lib/mcxx \
    $(END)
endif

# Silent rules
if SUPPORTED_SILENT_RULES
BISON_verbose = $(BISON_verbose_$(V))
BISON_verbose_ = $(BISON_verbose_$(AM_DEFAULT_VERBOSITY))
BISON_verbose_0 = @echo "  BISON " $@;

FLEX_verbose = $(FLEX_verbose_$(V))
FLEX_verbose_ = $(FLEX_verbose_$(AM_DEFAULT_VERBOSITY))
FLEX_verbose_0 = @echo "  FLEX  " $@;

GPERF_verbose = $(GPERF_verbose_$(V))
GPERF_verbose_ = $(GPERF_verbose_$(AM_DEFAULT_VERBOSITY))
GPERF_verbose_0 = @echo "  GPERF " $@;
endif

EXTRA_DIST += cxx-configoptions.gperf
EXTRA_DIST += cxx-fileextensions.gperf
EXTRA_DIST += cxx-debugflags.gperf

EXTRA_DIST += cxx-configfile-parser.y
EXTRA_DIST += cxx-configfile-lexer.l

BUILT_SOURCES += cxx-configoptions.c
BUILT_SOURCES += cxx-fileextensions.c
BUILT_SOURCES += cxx-debugflags.c


BUILT_SOURCES += cxx-configfile-parser.c
BUILT_SOURCES += cxx-configfile-parser-internal.h
BUILT_SOURCES += cxx-configfile-lexer.c

if GPERF_BUILDING
CLEANFILES += cxx-configoptions.c
CLEANFILES += cxx-debugflags.c
CLEANFILES += cxx-fileextensions.c
endif

CLEANFILES += cxx-configfile-lexer.c
CLEANFILES += cxx-configfile-parser.c
CLEANFILES += cxx-configfile-parser-internal.h
CLEANFILES += cxx-configfile-parser.output

cxx-configoptions.c : $(srcdir)/cxx-configoptions.gperf $(top_srcdir)/src/driver/cxx-driver.h $(srcdir)/cxx-configfile.h
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=cxx-configoptions.c \
      --initializer-suffix=,NULL $(srcdir)/cxx-configoptions.gperf
else
	@echo "*** ERROR: file cxx-configoptions.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

cxx-fileextensions.c : $(srcdir)/cxx-fileextensions.gperf $(top_srcdir)/src/driver/cxx-driver.h
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=fileextensions_hash --struct-type \
      --lookup-function-name=fileextensions_lookup --output=cxx-fileextensions.c \
      --initializer-suffix=,SOURCE_LANGUAGE_UNKNOWN,SOURCE_KIND_UNKNOWN $(srcdir)/cxx-fileextensions.gperf
else
	@echo "*** ERROR: file cxx-fileextensions.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

cxx-debugflags.c : $(srcdir)/cxx-debugflags.gperf $(top_srcdir)/src/driver/cxx-driver.h $(top_srcdir)/src/frontend/cxx-utils.h
if GPERF_BUILDING
	$(GPERF_verbose)$(GPERF) --language=ANSI-C --hash-function-name=debugflags_hash --struct-type \
      --lookup-function-name=debugflags_lookup --output=cxx-debugflags.c \
      --global-table --word-array-name=debugflags_list \
	  "--initializer-suffix=,0,\"\"" $(srcdir)/cxx-debugflags.gperf
else
	@echo "*** ERROR: file cxx-debugflags.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

cxx-configfile-lexer.c: $(srcdir)/cxx-configfile-lexer.l
	$(FLEX_verbose)$(FLEX) -Pconfigfile -d -ocxx-configfile-lexer.c -8 $(srcdir)/cxx-configfile-lexer.l

cxx-configfile-parser-internal.h : cxx-configfile-parser.c

cxx-configfile-parser.c: $(srcdir)/cxx-configfile-parser.y
	$(BISON_verbose)$(BISON) -pconfigfile --debug --defines=cxx-configfile-parser-internal.h --report=all --output=cxx-configfile-parser.c $(srcdir)/cxx-configfile-parser.y

# Install hooks
# installs "mcc" as a soft link of mcxx

        @list='$(mcxxexec_PROGRAMS)'; for p in $$list; do \
          p1=`echo $$p|sed 's/$(EXEEXT)$$//'`; \
          if test -f $$p \
             || test -f $$p1 \
          ; then \
            f=`echo "$$p1" | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'`; \
           echo " $(INSTALL_PROGRAM_ENV) $(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(mcxxexecPROGRAMS_INSTALL) '$$p' '$(DESTDIR)$(mcxxexecdir)/$$f'"; \
           $(INSTALL_PROGRAM_ENV) $(LIBTOOL) $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) --mode=install $(mcxxexecPROGRAMS_INSTALL) "$$p" "$(DESTDIR)$(mcxxexecdir)/$$f" || exit 1; \
          else :; fi; \
        done


install-exec-hook: 
	real_mcxx_name=`echo mcxx | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
	for compiler_name in @COMPILER_NAMES@; \
		do \
			real_compiler_name=`echo $${compiler_name} | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
			(cd $(DESTDIR)/$(mcxxexecdir); test -e $${real_compiler_name} || $(LN_S) $${real_mcxx_name} $${real_compiler_name}) ; \
		done
	real_mcxx_embedder_name=`echo mcxx-embedder | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
	real_mcc_embedder_name=`echo mcxx-embedder | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
	(cd $(DESTDIR)/$(mcxxexecdir); test -e $${real_mcc_embedder_name} || $(LN_S) $${real_mcxx_embedder_name} $${real_mcc_embedder_name} ) 
	
uninstall-local:
	for compiler_name in @COMPILER_NAMES@; \
		do \
			real_compiler_name=`echo $${compiler_name} | sed 's,^.*/,,;$(transform);s/$$/$(EXEEXT)/'` ; \
			rm -f $(DESTDIR)/$(mcxxexecdir)/$${real_compiler_name} ; \
		done


# Embedder

mcxx_embedder_CFLAGS=\
			-I$(top_srcdir)/lib \
			-I$(top_srcdir)/src/frontend \
			-I$(top_builddir)/lib \
			-I$(top_builddir)/src/frontend \
			-DPKGDATADIR=\"$(pkgdatadir)\" \
			-Wall

mcxx_embedder_SOURCES = \
                        cxx-embedder.c \
                        cxx-embed.c \
                        cxx-embed.h \
                        cxx-driver-utils.h \
                        cxx-driver-utils.c \
                        cxx-parameters.h \
                        cxx-parameters.c \
                        $(END)

mcxx_embedder_LDADD = \
   $(top_builddir)/gnulib/libgnulib.la \
   $(top_builddir)/lib/libmcxx-utils.la \
   $(top_builddir)/src/frontend/libmcxx-process.la \
   $(END)

mcxx_embedder_LDFLAGS = \
    -Wl,@RPATH_PARAMETER@,'$$ORIGIN'/../lib \
    $(END)
