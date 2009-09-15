include ../Makefile.phases

tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../omp_core
END=

phases_LTLIBRARIES = libtlomp-nanox.la

libtlomp_nanox_la_CFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_CXXFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_LDFLAGS = $(tl_ldflags)

libtlomp_nanox_la_SOURCES = \
     tl-omp-nanox.cpp \
     tl-omp-nanox.hpp \
     tl-parallel.cpp \
     tl-parallel-common.hpp \
     tl-parallel-common.cpp \
     tl-task.cpp \
     tl-taskwait.cpp \
     tl-data-env.hpp \
     tl-data-env.cpp \
     tl-outline-nanox.hpp \
     tl-outline-nanox.cpp \
     $(END)

