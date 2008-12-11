lib_LTLIBRARIES = libtlomp.la libtlomp_serialize.la libtlomp_taskaggregation.la

tl_omp_flags = -Wall \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/src/tl/nanos \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/src/tl/nanos \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/lib

libtlomp_la_CFLAGS = $(tl_omp_flags)
libtlomp_la_CXXFLAGS = $(tl_omp_flags)

END=

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
   tl-warning.cpp \
   tl-ordered.cpp \
   tl-outline.cpp \
   tl-parallel.cpp \
   tl-parallel_for.cpp \
   tl-parallel_sections.cpp \
   tl-parameterinfo.hpp \
   tl-reduction.cpp \
   tl-section.cpp \
   tl-sections.cpp \
   tl-single.cpp \
   tl-spawn.cpp \
   tl-spawn-old.cpp \
   tl-task.cpp \
   tl-taskwait.cpp \
   tl-taskyield.cpp \
   tl-taskgroup.cpp \
   tl-threadprivate.cpp \
   tl-taskserialize.hpp \
   tl-transaction.cpp \
   tl-adf.cpp \
   tl-transaction-expression.hpp \
   tl-transaction-expression.cpp \
   $(END)

libtlomp_la_LDFLAGS = -avoid-version

libtlomp_serialize_la_CFLAGS = $(tl_omp_flags)
libtlomp_serialize_la_CXXFLAGS = $(tl_omp_flags)

libtlomp_serialize_la_LDFLAGS = -avoid-version

libtlomp_serialize_la_SOURCES = \
   tl-ompserialize.hpp \
   tl-ompserialize.cpp \
   tl-taskserialize.hpp \
   $(END)

libtlomp_taskaggregation_la_CFLAGS = $(tl_omp_flags)
libtlomp_taskaggregation_la_CXXFLAGS = $(tl_omp_flags)

libtlomp_taskaggregation_la_LDFLAGS = -avoid-version

libtlomp_taskaggregation_la_SOURCES = \
   tl-taskaggregation.hpp \
   tl-taskaggregation.cpp \
   $(END)
