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

mcxx_LDFLAGS = \
   -L. \
   -L$(top_builddir)/lib \
   -L$(top_builddir)/src/frontend \
   -L$(top_builddir)/src/tl \
   -L$(top_builddir)/src/mcxx_tl \
   -Wl,-E

mcxx_LDADD = \
   -ldl \
   -lmcxx \
   -lmcxxtl \
   -ltl \
   -lmcxx-utils \
   -lmcxx-extstruct \
   -lmcxx-mcfg

EXTRA_DIST += cxx-configoptions.gperf
EXTRA_DIST += cxx-fileextensions.gperf
EXTRA_DIST += cxx-debugflags.gperf

if GPERF_BUILDING
CLEANFILES += cxx-configoptions.c
cxx-configoptions.c : $(srcdir)/cxx-configoptions.gperf $(top_srcdir)/src/driver/cxx-driver.h $(srcdir)/cxx-configfile.h
	gperf --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=cxx-configoptions.c \
      $(srcdir)/cxx-configoptions.gperf

CLEANFILES += cxx-fileextensions.c
cxx-fileextensions.c : $(srcdir)/cxx-fileextensions.gperf $(top_srcdir)/src/driver/cxx-driver.h
	gperf --language=ANSI-C --hash-function-name=fileextensions_hash --struct-type \
      --lookup-function-name=fileextensions_lookup --output=cxx-fileextensions.c \
      $(srcdir)/cxx-fileextensions.gperf

CLEANFILES += cxx-debugflags.c
cxx-debugflags.c : $(srcdir)/cxx-debugflags.gperf $(top_srcdir)/src/driver/cxx-driver.h $(top_srcdir)/src/frontend/cxx-utils.h
	gperf --language=ANSI-C --hash-function-name=debugflags_hash --struct-type \
      --lookup-function-name=debugflags_lookup --output=cxx-debugflags.c \
      --global-table --word-array-name=debugflags_list\
	  $(srcdir)/cxx-debugflags.gperf
endif

# Install hooks
# installs "mcc" as a soft link of mcxx
install-exec-hook: 
	-(cd $(DESTDIR)/$(mcxxexecdir); $(LN_S) mcxx mcc)

uninstall-local:
	rm -f $(DESTDIR)/$(mcxxexecdir)/mcc

