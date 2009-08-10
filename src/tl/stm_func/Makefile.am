include ../Makefile.phases

phases_LTLIBRARIES = libtlstmfunc.la

libtlstmfunc_la_CFLAGS = -Wall \
                         $(tl_cflags)

libtlstmfunc_la_CXXFLAGS = -Wall \
                         $(tl_cxxflags)

libtlstmfunc_la_SOURCES = \
    stm_function_transform.hpp \
    stm_function_transform.cpp \
	tl-functionfilter.cpp \
	tl-functionfilter.hpp

libtlstmfunc_la_LDFLAGS = $(tl_ldflags)
libtlstmfunc_la_LIBADD = $(tl_libadd)
