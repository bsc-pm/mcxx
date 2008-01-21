lib_LTLIBRARIES = libtlomp.la

libtlomp_la_CFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlomp_la_CXXFLAGS = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlomp_la_SOURCES = \
   tl-omptransform.cpp \
   tl-omptransform.hpp \
   tl-atomic.cpp \
   tl-barrier.cpp \
   tl-critical.cpp \
   tl-flush.cpp \
   tl-for.cpp \
   tl-functionfilter.cpp \
   tl-functionfilter.hpp \
   tl-loop.cpp \
   tl-master.cpp \
   tl-datasharing.cpp \
   tl-debug.cpp \
   tl-symbols.cpp \
   tl-transaction.cpp \
   tl-warning.cpp \
   tl-ordered.cpp \
   tl-outline.cpp \
   tl-parallel.cpp \
   tl-parallel_for.cpp \
   tl-parallel_sections.cpp \
   tl-parallel_single.cpp \
   tl-parameterinfo.hpp \
   tl-reduction.cpp \
   tl-section.cpp \
   tl-sections.cpp \
   tl-single.cpp \
   tl-spawn.cpp \
   tl-task.cpp \
   tl-threadprivate.cpp
