lib_LTLIBRARIES = libtlbasic_test.la

COMMON_CFLAGS= -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlbasic_test_la_CXXFLAGS = $(COMMON_CFLAGS)

libtlbasic_test_la_SOURCES = \
						tl-basic_test.hpp \
						tl-basic_test.cpp

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

libtlbasic_test_la_LIBADD = $(top_builddir)/src/tl/libtl.la \
                            $(top_builddir)/src/frontend/libmcxx.la

libtlbasic_test_la_LDFLAGS = -avoid-version $(no_undefined)
