include ../Makefile.phases

phases_LTLIBRARIES = libtlinstr-pthreads.la

libtlinstr_pthreads_la_CFLAGS = -Wall \
                                $(tl_cflags)

libtlinstr_pthreads_la_CXXFLAGS = -Wall \
                                  $(tl_cxxflags)

libtlinstr_pthreads_la_LDFLAGS = $(tl_ldflags)
libtlinstr_pthreads_la_LIBADD = $(tl_libadd)

libtlinstr_pthreads_la_SOURCES = \
						tl-instrumentfilter.hpp \
						tl-instrumentfilter.cpp \
						tl-instrumentcalls.hpp \
						tl-instrumentcalls.cpp \
						tl-instrumentation.hpp \
						tl-instrumentation.cpp
