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

phases_LTLIBRARIES = libextensible_graph.la

libextensible_graph_la_CFLAGS = $(tl_cflags)
libextensible_graph_la_CXXFLAGS = $(tl_cflags)
libextensible_graph_la_LDFLAGS = $(tl_ldflags)
libextensible_graph_la_LIBADD = $(tl_libadd)

END=

EXTRA_DIST =

libextensible_graph_la_SOURCES = \
   edge.hpp \
   edge.cpp \
   node.hpp \
   node.cpp \
   extensible_graph.cpp \
   extensible_graph.hpp \
   dot_graph.cpp \
   analysis_phase.hpp \
   analysis_phase.cpp \
   $(END)
