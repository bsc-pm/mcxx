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

noinst_LTLIBRARIES = libtlnanos-common.la

include ../../Makefile.phases

phases_LTLIBRARIES = libtlnanos-version.la

END =

#### libtl-nanos4.la

libtlnanos_version_la_SOURCES = \
	tl-nanos.hpp \
	tl-nanos.cpp \
	$(END)

libtlnanos_version_la_CFLAGS = $(tl_cflags)
libtlnanos_version_la_CXXFLAGS = $(tl_cxxflags)

libtlnanos_version_la_LDFLAGS = $(tl_ldflags)
libtlnanos_version_la_LIBADD = $(tl_libadd)

#### libtl-nanos-common.la

libtlnanos_common_la_CXXFLAGS=$(tl_cxxflags)

libtlnanos_common_la_SOURCES=\
           tl-nanos-atomic.hpp \
           tl-nanos-atomic.cpp \
           $(END)
