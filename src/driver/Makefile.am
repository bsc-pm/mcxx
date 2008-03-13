# mcxxexec can't be changed to simply 'bin' or 'mcxx' (otherwise things won't
# work)
mcxxexec_PROGRAMS = mcxx

if ENABLE_PKGLIB_INSTALL
mcxxexecdir = $(pkglibdir)
else
mcxxexecdir = @bindir@
endif

EXTRA_DIST =
CLEANFILES = 

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
  cxx-driver.h

# mcxx_LDFLAGS = \
#    -L. \
#    -L$(top_builddir)/lib \
#    -L$(top_builddir)/src/frontend \
#    -L$(top_builddir)/src/tl \
#    -L$(top_builddir)/src/mcxx_tl \
#    -Wl,-E

END=

mcxx_LDADD = \
   $(top_builddir)/lib/libmcxx-utils.la \
   $(top_builddir)/lib/libmcxx-extstruct.la \
   $(top_builddir)/lib/libmcxx-mcfg.la \
   $(top_builddir)/src/frontend/libmcxx.la \
   $(top_builddir)/src/tl/libtl.la \
   $(top_builddir)/src/mcxx_tl/libmcxxtl.la \
   -ldl \
   $(END)

EXTRA_DIST += cxx-configoptions.gperf
EXTRA_DIST += cxx-fileextensions.gperf
EXTRA_DIST += cxx-debugflags.gperf

if GPERF_BUILDING
CLEANFILES += cxx-configoptions.c
endif

cxx-configoptions.c : $(srcdir)/cxx-configoptions.gperf $(top_srcdir)/src/driver/cxx-driver.h $(srcdir)/cxx-configfile.h
	gperf --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=cxx-configoptions.c \
      --initializer-suffix=,NULL $(srcdir)/cxx-configoptions.gperf

if GPERF_BUILDING
CLEANFILES += cxx-fileextensions.c
endif
cxx-fileextensions.c : $(srcdir)/cxx-fileextensions.gperf $(top_srcdir)/src/driver/cxx-driver.h
if GPERF_BUILDING
	gperf --language=ANSI-C --hash-function-name=fileextensions_hash --struct-type \
      --lookup-function-name=fileextensions_lookup --output=cxx-fileextensions.c \
      --initializer-suffix=,SOURCE_LANGUAGE_UNKNOWN,SOURCE_KIND_UNKNOWN $(srcdir)/cxx-fileextensions.gperf
else
	@echo "*** ERROR: file cxx-fileextensions.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

if GPERF_BUILDING
CLEANFILES += cxx-debugflags.c
endif
cxx-debugflags.c : $(srcdir)/cxx-debugflags.gperf $(top_srcdir)/src/driver/cxx-driver.h $(top_srcdir)/src/frontend/cxx-utils.h
if GPERF_BUILDING
	gperf --language=ANSI-C --hash-function-name=debugflags_hash --struct-type \
      --lookup-function-name=debugflags_lookup --output=cxx-debugflags.c \
      --global-table --word-array-name=debugflags_list \
	  "--initializer-suffix=,0,\"\"" $(srcdir)/cxx-debugflags.gperf
else
	@echo "*** ERROR: file cxx-debugflags.gperf was modified but no suitable gperf was found during configure ***"
	@exit 1
endif

# Install hooks
# installs "mcc" as a soft link of mcxx
install-exec-hook: 
	for compiler_name in @COMPILER_NAMES@; \
		do \
			(cd $(DESTDIR)/$(mcxxexecdir); test -e $${compiler_name} || $(LN_S) mcxx $${compiler_name}) ; \
		done
	
uninstall-local:
	for compiler_name in @COMPILER_NAMES@; \
		do \
			rm -f $(DESTDIR)/$(mcxxexecdir)/$${compiler_name} ; \
		done


