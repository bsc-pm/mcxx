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

lib_LTLIBRARIES = libmcxxtl.la

# Gluing library C to C++
libmcxxtl_la_SOURCES = \
	cxx-compilerphases.hpp \
	cxx-compilerphases.cpp

COMMON_CFLAGS= -DLIBMCXXTL_DLL_EXPORT \
    -I $(top_srcdir)/src/frontend \
    -I $(top_srcdir)/src/driver \
    -I $(top_srcdir)/src/tl \
    -I $(top_srcdir)/lib \
    -I $(top_builddir)/src/frontend \
    -I $(top_builddir)/src/driver \
    -I $(top_builddir)/src/tl \
    -I $(top_builddir)/lib

libmcxxtl_la_CXXFLAGS = $(COMMON_CFLAGS)

libmcxxtl_la_CFLAGS = $(COMMON_CFLAGS)

END=

if WINDOWS_BUILD
no_undefined=-no-undefined
DL_LIBRARY=
else
no_undefined=
DL_LIBRARY=-ldl
endif

libmcxxtl_la_LIBADD = \
        $(top_builddir)/src/frontend/libmcxx-process.la \
        $(top_builddir)/src/frontend/libmcxx.la \
        $(top_builddir)/src/tl/libtl.la \
	    $(top_builddir)/lib/libmcxx-utils.la \
        $(DL_LIBRARY)
		$(END)
	
libmcxxtl_la_LDFLAGS=-avoid-version $(no_undefined) \
    -Wl,-R,$(pkglibdir) \
    $(END)
