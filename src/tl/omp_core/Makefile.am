include ../Makefile.phases

phases_LTLIBRARIES = libtlomp-core.la

libtlomp_core_la_CFLAGS = $(tl_cflags)
libtlomp_core_la_CXXFLAGS = $(tl_cflags)

libtlomp_core_la_LDFLAGS = $(tl_ldflags)
libtlomp_core_la_LIBADD = $(tl_libadd)

END=

EXTRA_DIST =

EXTRA_DIST += tl-omp-constructs.def

libtlomp_core_la_SOURCES = \
   tl-omp-core.hpp \
   tl-omp-core.cpp \
   tl-omp.cpp \
   tl-omp.hpp \
   $(END)
