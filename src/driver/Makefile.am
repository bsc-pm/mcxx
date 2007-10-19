bin_PROGRAMS = mcxx

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

if GPERF_BUILDING
CLEANFILES += cxx-configoptions.c
cxx-configoptions.c : $(srcdir)/cxx-configoptions.gperf $(top_srcdir)/src/driver/cxx-driver.h $(srcdir)/cxx-configfile.h
	gperf --language=ANSI-C --hash-function-name=configoptions_hash --struct-type \
      --lookup-function-name=configoptions_lookup --output=cxx-configoptions.c \
      $(srcdir)/cxx-configoptions.gperf
endif

# Install hooks
# installs "mcc" as a copy of mcxx
.PHONY : mcc
mcc :
install-exec-local:
	$(LIBTOOL) --mode=install --mode=install /usr/bin/install -c mcxx $(DESTDIR)/$(bindir)/mcc
