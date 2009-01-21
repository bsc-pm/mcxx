tl_omp_flags = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/tl/nanos \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/tl/nanos \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib


lib_LTLIBRARIES = libtlsizeof-check.la

libtlsizeof_check_la_CXXFLAGS = $(tl_omp_flags)

END=

libtlsizeof_check_la_SOURCES = \
                tl-test-sizeof.hpp \
                tl-test-sizeof.cpp \
                $(END)
