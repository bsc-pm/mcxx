##########################################################################
#  (C) Copyright 2006-2013 Barcelona Supercomputing Center               #
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

include $(top_srcdir)/src/tl/Makefile.phases

phases_LTLIBRARIES = libtlnanox-lowering.la

libtlnanox_lowering_la_CFLAGS = $(phases_cflags) -I$(srcdir)/../nanos-common -I$(srcdir)/../core -I @NANOX_INCLUDES@
libtlnanox_lowering_la_CXXFLAGS = $(phases_cflags) -I$(srcdir)/../nanos-common -I$(srcdir)/../core -I @NANOX_INCLUDES@

libtlnanox_lowering_la_LIBADD = $(phases_libadd) \
								../core/libtlomp-core.la \
								../nanos-common/libtlnanos-version.la
libtlnanox_lowering_la_LDFLAGS = $(phases_ldflags)

END=

libtlnanox_lowering_la_SOURCES = \
								 tl-nanox-nodecl.cpp \
								 tl-nanox-nodecl.hpp \
								 tl-nanox-nodecl-prerun.cpp \
								 tl-lowering-visitor.cpp \
								 tl-lowering-visitor.hpp \
								 tl-lower-arguments.cpp \
								 tl-lower-task-common.hpp \
								 tl-lower-task.cpp \
								 tl-lower-task-expression.cpp \
								 tl-lower-task-call.cpp \
								 tl-lower-taskwait.cpp \
								 tl-outline-info.hpp \
								 tl-outline-info.cpp \
								 tl-lower-single.cpp \
								 tl-lower-workshare.cpp \
								 tl-lower-master.cpp \
								 tl-lower-barrier.cpp \
								 tl-lower-parallel.cpp \
								 tl-lower-for.cpp \
								 tl-lower-for-worksharing.cpp \
								 tl-lower-for-slicer.cpp \
								 tl-lower-sections.cpp \
								 tl-lower-critical.cpp \
								 tl-lower-flush.cpp \
								 tl-lower-atomic.cpp \
								 tl-lower-target.cpp \
								 tl-parallel-spawn.cpp \
								 tl-loop-spawn-worksharing.cpp \
								 tl-loop-spawn-slicer.cpp \
								 tl-reduction-openmp.cpp \
								 tl-reduction-ompss.cpp \
								 tl-nanox-ptr.cpp \
								 tl-devices.cpp \
								 tl-devices.hpp \
								 tl-target-information.cpp \
								 tl-target-information.hpp \
								 $(END)

