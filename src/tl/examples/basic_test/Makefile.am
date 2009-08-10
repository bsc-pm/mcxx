include ../../Makefile.phases

phases_LTLIBRARIES = libtlbasic_test.la

COMMON_CFLAGS= -Wall $(tl_cflags)

libtlbasic_test_la_CXXFLAGS = $(COMMON_CFLAGS)

libtlbasic_test_la_SOURCES = \
						tl-basic_test.hpp \
						tl-basic_test.cpp


libtlbasic_test_la_LIBADD = $(tl_libadd)
libtlbasic_test_la_LDFLAGS = $(tl_ldflags)
