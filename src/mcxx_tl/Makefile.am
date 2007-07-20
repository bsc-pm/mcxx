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

libmcxxtl_la_LDFLAGS = \
   -L$(top_builddir)/lib \
   -L$(top_builddir)/src/frontend \
   -L$(top_builddir)/src/tl

libmcxxtl_la_LIBADD = \
		-ltl \
		-lmcxx-utils \
		-lmcxx
	
