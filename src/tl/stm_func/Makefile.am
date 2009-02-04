lib_LTLIBRARIES = libtlstmfunc.la

libtlstmfunc_la_CFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlstmfunc_la_CXXFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlstmfunc_la_SOURCES = \
    stm_function_transform.hpp \
    stm_function_transform.cpp \
	tl-functionfilter.cpp \
	tl-functionfilter.hpp

if WINDOWS_BUILD
no_undefined=-no-undefined
else
no_undefined=
endif

libtlstmfunc_la_LDFLAGS = -avoid-version $(no_undefined)
