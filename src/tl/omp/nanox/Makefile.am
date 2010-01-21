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

tl_omp_flags = -Wall $(tl_cflags) -I $(srcdir)/../core
END=

phases_LTLIBRARIES = libtlomp-nanox.la

libtlomp_nanox_la_CFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_CXXFLAGS = $(tl_omp_flags)
libtlomp_nanox_la_LDFLAGS = $(tl_ldflags)

libtlomp_nanox_la_SOURCES = \
     tl-omp-nanox.cpp \
     tl-omp-nanox.hpp \
     tl-parallel.cpp \
     tl-parallel-common.hpp \
     tl-parallel-common.cpp \
     tl-single.cpp \
     tl-task.cpp \
     tl-taskwait.cpp \
     tl-for.cpp \
     tl-data-env.hpp \
     tl-data-env.cpp \
     tl-outline-nanox.hpp \
     tl-outline-nanox.cpp \
     tl-threadprivate.cpp \
     tl-atomic.cpp \
     $(END)

