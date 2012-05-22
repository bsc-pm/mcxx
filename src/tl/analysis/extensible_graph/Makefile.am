##########################################################################
#  (C) Copyright 2006-2012 Barcelona Supercomputing Center               #
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

include ../../Makefile.phases

phases_LTLIBRARIES = libextensible_graph.la

libextensible_graph_la_CFLAGS = $(tl_cflags)
libextensible_graph_la_CXXFLAGS = $(tl_cflags) -I $(srcdir)/../../
libextensible_graph_la_LDFLAGS = $(tl_ldflags)
libextensible_graph_la_LIBADD = $(tl_libadd) ../../libtl.la

END=

EXTRA_DIST =

libextensible_graph_la_SOURCES = \
   tl-analysis-common.cpp \
   tl-analysis-common.hpp \
   tl-analysis-phase.cpp \
   tl-analysis-phase.hpp \
   tl-cfg-analysis-visitor.cpp \
   tl-cfg-analysis-visitor.hpp \
   tl-cfg-renaming-visitor.cpp \
   tl-cfg-renaming-visitor.hpp \
   tl-cfg-visitor.cpp \
   tl-cfg-visitor.hpp \
   tl-edge.cpp \
   tl-edge.hpp \
   tl-node.cpp \
   tl-node.hpp \
   tl-extensible-graph.cpp \
   tl-extensible-graph.hpp \
   tl-extensible-symbol.cpp \
   tl-extensible-symbol.hpp \
   tl-reaching-definitions.cpp \
   tl-iv-analysis.cpp \
   tl-loop-analysis.cpp \
   tl-loop-analysis.hpp \
   tl-static-analysis.cpp \
   tl-static-analysis.hpp \
   tl-structures.hpp \
   tl-dot-graph.cpp \
   $(END)

