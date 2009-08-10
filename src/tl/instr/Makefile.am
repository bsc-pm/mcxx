include ../Makefile.phases

phases_LTLIBRARIES = libtlinstr.la

libtlinstr_la_CFLAGS = -Wall \
                       $(tl_cflags)

libtlinstr_la_CXXFLAGS = -Wall \
                       $(tl_cxxflags)

libtlinstr_la_SOURCES = \
						tl-instrumentfilter.hpp \
						tl-instrumentfilter.cpp \
						tl-instrumentcalls.hpp \
						tl-instrumentcalls.cpp \
						tl-instrumentation.hpp \
						tl-instrumentation.cpp

libtlinstr_la_LDFLAGS = $(tl_ldflags)
libtlinstr_la_LIBADD = $(tl_libadd)
