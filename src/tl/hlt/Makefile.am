lib_LTLIBRARIES = libtl-hlt.la libtl-hlt-pragma.la

include ../Makefile.phases

END=

COMMON_CFLAGS=-DLIBHLT_DLL_EXPORT

libtl_hlt_la_CFLAGS=$(tl_cflags) $(COMMON_CFLAGS)
libtl_hlt_la_CXXFLAGS=$(tl_cxxflags) $(COMMON_CFLAGS)
libtl_hlt_la_LIBADD=$(tl_libadd)
libtl_hlt_la_LDFLAGS=$(tl_ldflags)

libtl_hlt_la_SOURCES = \
      hlt-transform.hpp \
      hlt-transform.cpp \
      hlt-exception.hpp \
      hlt-exception.cpp \
      hlt-unroll.hpp \
      hlt-unroll.cpp \
      hlt-unroll-omp.hpp \
      hlt-unroll-omp.cpp \
      hlt-blocking.hpp \
      hlt-blocking.cpp \
      hlt-distribution.hpp \
      hlt-distribution.cpp \
      hlt-fusion.hpp \
      hlt-fusion.cpp \
      hlt-interchange.cpp \
      hlt-interchange.hpp \
      hlt-collapse.cpp \
      hlt-collapse.hpp \
      hlt-composition.cpp \
      hlt-composition.hpp \
      hlt-outline.hpp \
      hlt-outline.cpp \
      hlt-extension.hpp \
      hlt-extension.cpp \
      hlt-peeling.hpp \
      hlt-peeling.cpp \
      hlt-common.hpp \
      hlt-task-aggregation.hpp \
      hlt-task-aggregation.cpp \
      hlt-task-aggregation-common.hpp \
      hlt-task-aggregation-simple.cpp \
      hlt-task-aggregation-predication.cpp \
      hlt-task-aggregation-bundling.cpp \
      $(END)

libtl_hlt_pragma_la_CFLAGS=$(tl_cflags)
libtl_hlt_pragma_la_CXXFLAGS=$(tl_cxxflags)
libtl_hlt_pragma_la_LIBADD=$(tl_libadd) libtl-hlt.la
libtl_hlt_pragma_la_LDFLAGS=$(tl_ldflags) 

libtl_hlt_pragma_la_SOURCES = \
      hlt-exception.hpp \
      hlt-pragma.cpp \
      hlt-pragma.hpp \
      $(END)
