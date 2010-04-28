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

include ../Makefile.phases

phases_LTLIBRARIES = libtl-ss-valgrind.la

libtl_ss_valgrind_la_SOURCES = \
       tl-ss-valgrind.hpp \
       tl-ss-valgrind.cpp \
       $(END)

SS_FLAGS=-I $(top_srcdir)/src/tl/superscalar

libtl_ss_valgrind_la_CFLAGS=$(tl_cflags) $(SS_FLAGS)
libtl_ss_valgrind_la_CXXFLAGS=$(tl_cxxflags) $(SS_FLAGS)
libtl_ss_valgrind_la_LDFLAGS=$(tl_ldflags)
libtl_ss_valgrind_la_LIBADD=$(tl_libadd)

