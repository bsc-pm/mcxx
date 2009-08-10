include ../../Makefile.phases

phases_LTLIBRARIES = libtlsizeof-check.la

libtlsizeof_check_la_CXXFLAGS = -Wall $(tl_cxxflags)
libtlsizeof_check_la_LDFLAGS = $(tl_ldflags)
libtlsizeof_check_la_LIBADD = $(tl_libadd)

END=

libtlsizeof_check_la_SOURCES = \
                tl-test-sizeof.hpp \
                tl-test-sizeof.cpp \
                $(END)
