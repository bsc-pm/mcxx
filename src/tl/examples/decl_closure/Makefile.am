lib_LTLIBRARIES = libtldecl_closure.la

libtldecl_closure_la_CFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtldecl_closure_la_CXXFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtldecl_closure_la_SOURCES = \
						tl-decl_closure.hpp \
						tl-decl_closure.cpp

libtldecl_closure_la_LIBADD = \
                        $(top_builddir)/src/tl/libtl.la \
                        $(top_builddir)/src/frontend/libmcxx.la

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

libtldecl_closure_la_LDFLAGS = -avoid-version $(no_undefined)
