lib_LTLIBRARIES = libmcxxtl.la

# Gluing library C to C++
libmcxxtl_la_SOURCES = \
	cxx-compilerphases.hpp \
	cxx-compilerphases.cpp

COMMON_CFLAGS= -DLIBMCXXTL_DLL_EXPORT \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/lib

libmcxxtl_la_CXXFLAGS = $(COMMON_CFLAGS)

libmcxxtl_la_CFLAGS = $(COMMON_CFLAGS)

END=

if WINDOWS_BUILD
no_undefined=-no-undefined
DL_LIBRARY=
else
no_undefined=
DL_LIBRARY=-ldl
endif

libmcxxtl_la_LIBADD = \
        $(top_builddir)/src/frontend/libmcxx-process.la \
        $(top_builddir)/src/frontend/libmcxx.la \
        $(top_builddir)/src/tl/libtl.la \
	    $(top_builddir)/lib/libmcxx-utils.la \
        $(DL_LIBRARY)
		$(END)
	
libmcxxtl_la_LDFLAGS=-avoid-version $(no_undefined) \
    -Wl,-rpath,$(pkglibdir) \
    $(END)
