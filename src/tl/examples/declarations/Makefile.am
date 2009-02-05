lib_LTLIBRARIES = libtltraversedecls.la

libtltraversedecls_la_SOURCES = \
	tl-traversedecls.cpp

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

libtltraversedecls_la_LDFLAGS = -avoid-version $(no_undefined)
libtltraversedecls_la_LIBADD = $(top_builddir)/src/tl/libtl.la \
                         $(top_builddir)/src/frontend/libmcxx.la

libtltraversedecls_la_CFLAGS = -Wall \
    -i $(top_srcdir)/src/tl \
    -i $(top_srcdir)/src/frontend \
    -i $(top_srcdir)/src/driver \
    -i $(top_srcdir)/lib \
    -i $(top_builddir)/src/tl \
    -i $(top_builddir)/src/frontend \
    -i $(top_builddir)/src/driver \
    -i $(top_builddir)/lib

libtltraversedecls_la_CXXFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib
