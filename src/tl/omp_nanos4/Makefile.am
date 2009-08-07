include ../Makefile.phases

lib_LTLIBRARIES = libtlnanos4.la libtlomp-nanos4.la libtlomp_serialize.la 

tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../nanos4 -I $(srcdir)/../omp_core

libtlomp_nanos4_la_CFLAGS = $(tl_omp_flags)
libtlomp_nanos4_la_CXXFLAGS = $(tl_omp_flags)

END=

libtlnanos4_la_LDFLAGS = $(tl_ldflags)
libtlnanos4_la_LIBADD = $(tl_libadd)

libtlnanos4_la_SOURCES = \
	tl-nanos.hpp \
	tl-nanos.cpp \
	$(END)

libtlomp_nanos4_la_SOURCES = \
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
   tl-declare.cpp \
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
   tl-pretransform.hpp \
   tl-pretransform.cpp \
   $(END)

libtlomp_nanos4_la_LDFLAGS = $(tl_ldflags)
libtlomp_nanos4_la_LIBADD = $(tl_libadd) ../omp_core/libtlomp-core.la

libtlomp_serialize_la_CFLAGS = $(tl_omp_flags)
libtlomp_serialize_la_CXXFLAGS = $(tl_omp_flags)

libtlomp_serialize_la_LDFLAGS = $(tl_ldflags)
libtlomp_serialize_la_LIBADD = $(tl_libadd) ../omp_core/libtlomp-core.la

libtlomp_serialize_la_SOURCES = \
   tl-ompserialize.hpp \
   tl-ompserialize.cpp \
   tl-taskserialize.hpp \
   $(END)

