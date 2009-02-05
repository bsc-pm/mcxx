lib_LTLIBRARIES = libtlmypragma.la

libtlmypragma_la_CFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlmypragma_la_CXXFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlmypragma_la_SOURCES = \
						tl-mypragma.hpp \
						tl-mypragma.cpp

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

libtlmypragma_la_LDFLAGS = -avoid-version $(no_undefined)
libtlmypragma_la_LIBADD= $(top_builddir)/src/tl/libtl.la \
                         $(top_builddir)/src/frontend/libmcxx.la
