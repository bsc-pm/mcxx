##########################################################################
#  (C) Copyright 2006-2011 Barcelona Supercomputing Center               #
#                          Centro Nacional de Supercomputacion           #
#                                                                        #
#  This file is part of Mercurium C/C++ source-to-source compiler.       #
#                                                                        #
#  See AUTHORS file in the top level directory for information           #
#  regarding developers and contributors.                                #
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

SUBDIRS=. devices

include ../../Makefile.phases

tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../core -I $(srcdir)/../nanos-common -I @NANOX_INCLUDES@
END=

phases_LTLIBRARIES = libtlomp-nanox.la

libtlomp_nanox_la_CFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_CXXFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_LDFLAGS = $(tl_ldflags)
libtlomp_nanox_la_LIBADD = $(tl_libadd) ../core/libtlomp-core.la ../nanos-common/libtlnanos-common.la

libtlomp_nanox_la_SOURCES = \
     tl-omp-nanox.cpp \
     tl-omp-nanox.hpp \
     tl-omp-global.cpp \
     tl-parallel.cpp \
     tl-parallel-common.hpp \
     tl-parallel-common.cpp \
     tl-single.cpp \
     tl-task.cpp \
     tl-taskwait.cpp \
     tl-for.cpp \
     tl-data-env.hpp \
     tl-data-env.cpp \
     tl-threadprivate.cpp \
     tl-atomic.cpp \
     tl-barrier.cpp \
     tl-flush.cpp \
     tl-critical.cpp \
     tl-master.cpp \
     tl-sections.cpp \
     tl-section.cpp \
     tl-target.cpp \
     tl-devices.hpp \
     tl-devices.cpp \
     tl-omp-common.cpp \
     $(END)

