lib_LTLIBRARIES = libtl.la libtlomp.la

BUILT_SOURCES = ../cxx-asttype.h

libtl_la_CXXFLAGS = -Wall -O0 \
    -I $(srcdir)/../../src \
    -I $(srcdir)/../../lib \
    -I $(srcdir)/../../gc/include \
    -I ../../src \
    -I ../../lib

libtl_la_CFLAGS = -Wall -O0 \
    -I $(srcdir)/../../src \
    -I $(srcdir)/../../lib \
    -I $(srcdir)/../../gc/include \
    -I ../../src \
    -I ../../lib

libtl_la_SOURCES = \
    tl-object.hpp \
    tl-object.cpp \
    tl-builtin.hpp \
    tl-builtin.cpp \
    tl-ast.hpp \
    tl-ast.cpp \
    tl-predicate.hpp \
    tl-predicate.cpp \
    tl-scope.hpp \
    tl-scope.cpp \
	tl-scopelink.hpp \
	tl-scopelink.cpp \
    tl-dto.hpp \
    tl-dto.cpp \
	tl-symbol.hpp \
	tl-symbol.cpp \
    tl-compilerphase.hpp \
    tl-compilerphase.cpp \
	tl-source.hpp \
	tl-source.cpp

libtlomp_la_CXXFLAGS = -Wall -O0 \
    -I $(srcdir)/../../src \
    -I $(srcdir)/../../lib \
    -I $(srcdir)/../../gc/include \
    -I ../../src \
    -I ../../lib

libtlomp_la_CFLAGS = -Wall -O0 \
    -I $(srcdir)/../../src \
    -I $(srcdir)/../../lib \
    -I $(srcdir)/../../gc/include \
    -I ../../src \
    -I ../../lib

libtlomp_la_SOURCES = \
                        tl-omp.hpp \
                        tl-omp.cpp

../cxx-asttype.h :
	$(MAKE) -C .. cxx-asttype.h
