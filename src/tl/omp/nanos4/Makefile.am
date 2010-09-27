##########################################################################
#  (C) Copyright 2006-2009 Barcelona Supercomputing Center               #
#                          Centro Nacional de Supercomputacion           #
#                                                                        #
#  This file is part of Mercurium C/C++ source-to-source compiler.       #
#                                                                        #
#  This library is free software; you can redistribute it and/or         #
#  modify it under the terms of the GNU Lesser General Public            #
#  License as published by the Free Software Foundation; either          #
#  version 3 of the License, or (at your option) any later version.      #
#                                                                        #
#  Mercurium C/C++ source-to-source compiler is distributed in the hope  #
#  that it will be useful, but WITHOUT ANY WARRANTY; without even the    #
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR       #
#  PURPOSE.  See the GNU Lesser General Public License for more          #
#  details.                                                              #
#                                                                        #
#  You should have received a copy of the GNU Lesser General Public      #
#  License along with Mercurium C/C++ source-to-source compiler; if      #
#  not, write to the Free Software Foundation, Inc., 675 Mass Ave,       #
#  Cambridge, MA 02139, USA.                                             #
##########################################################################

include ../../Makefile.phases

phases_LTLIBRARIES = libtlomp-nanos4.la libtlomp-serialize.la 


END=
tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../core -I $(srcdir)/../nanos-common

#### libtl-omp-nanos4.la

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
   tl-target.cpp \
   $(END)


libtlomp_nanos4_la_CFLAGS = $(tl_omp_flags)
libtlomp_nanos4_la_CXXFLAGS = $(tl_omp_flags)

libtlomp_nanos4_la_LDFLAGS = $(tl_ldflags)
libtlomp_nanos4_la_LIBADD = $(tl_libadd) ../core/libtlomp-core.la ../nanos-common/libtlnanos-common.la

#### libtl-omp-serialize.la

libtlomp_serialize_la_CFLAGS = $(tl_omp_flags)
libtlomp_serialize_la_CXXFLAGS = $(tl_omp_flags)

libtlomp_serialize_la_LDFLAGS = $(tl_ldflags)
libtlomp_serialize_la_LIBADD = $(tl_libadd) ../core/libtlomp-core.la

libtlomp_serialize_la_SOURCES = \
   tl-ompserialize.hpp \
   tl-ompserialize.cpp \
   tl-taskserialize.hpp \
   $(END)

