include ../Makefile.phases

tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../omp_core
END=

lib_LTLIBRARIES = libtlomp-nanox.la

libtlomp_nanox_la_CFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_CXXFLAGS = $(tl_omp_flags)

libtlomp_nanox_la_SOURCES = \
     tl-omp-nanox.cpp \
     tl-omp-nanox.hpp \
     $(END)

