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


include $(top_srcdir)/src/tl/Makefile.phases

END=

phases_LTLIBRARIES=libtlnanox-opencl.la

device_flags=$(phases_cflags) -I$(srcdir)/../.. \
			 -I$(srcdir)/../../../nanos-common \
			 -I$(srcdir)/../../../core

libtlnanox_opencl_la_CFLAGS = $(device_flags)
libtlnanox_opencl_la_CXXFLAGS = $(device_flags)

libtlnanox_opencl_la_LDFLAGS = $(phases_ldflags)

libtlnanox_opencl_la_SOURCES = \
       nanox-opencl.hpp \
       nanox-opencl.cpp \
       $(END)
