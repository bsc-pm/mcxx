lib_LTLIBRARIES = libtl-hlt.la libtl-hlt-pragma.la

include ../Makefile.phases

END=

libtl_hlt_la_CFLAGS=$(tl_cflags)
libtl_hlt_la_CXXFLAGS=$(tl_cxxflags)
libtl_hlt_la_LIBADD=$(tl_libadd)
libtl_hlt_la_LDFLAGS=$(tl_ldflags)

libtl_hlt_la_SOURCES = \
      hlt-transform.hpp \
      hlt-transform.cpp \
      hlt-exception.hpp \
      hlt-unroll.hpp \
      hlt-unroll.cpp \
      hlt-blocking.hpp \
      hlt-blocking.cpp \
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
