lib_LTLIBRARIES = libtlinstr.la

libtlinstr_la_CFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlinstr_la_CXXFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlinstr_la_SOURCES = \
						tl-instrumentfilter.hpp \
						tl-instrumentfilter.cpp \
						tl-instrumentcalls.hpp \
						tl-instrumentcalls.cpp \
						tl-instrumentation.hpp \
						tl-instrumentation.cpp

libtlinstr_la_LDFLAGS = -avoid-version
