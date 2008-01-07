lib_LTLIBRARIES = libmcxxtl.la

# Gluing library C to C++
libmcxxtl_la_SOURCES = \
	cxx-compilerphases.hpp \
	cxx-compilerphases.cpp

libmcxxtl_la_CXXFLAGS = \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/lib

libmcxxtl_la_CFLAGS = \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/lib

END=

libmcxxtl_la_LIBADD = \
        $(top_builddir)/src/frontend/libmcxx.la \
        $(top_builddir)/src/tl/libtl.la \
	    $(top_builddir)/lib/libmcxx-utils.la \
		$(END)
	
