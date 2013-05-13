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

include $(top_srcdir)/src/tl/Makefile.tl

lib_LTLIBRARIES = libpcfg.la

COMMON_FLAGS= $(tl_cflags) -I $(srcdir)/../../ -I $(srcdir)/../common -I $(srcdir)/../loops

libpcfg_la_CFLAGS = $(COMMON_FLAGS)
libpcfg_la_CXXFLAGS = $(COMMON_FLAGS)
libpcfg_la_LDFLAGS = $(tl_ldflags)
libpcfg_la_LIBADD = $(tl_libadd) ../../libtl.la ../common/libanalysis_utils.la

END=

libpcfg_la_SOURCES= tl-pcfg-utils.hpp \
                    tl-pcfg-utils.cpp \
                    tl-edge.hpp \
                    tl-edge.cpp \
                    tl-node.hpp \
                    tl-node.cpp \
                    tl-extensible-graph.hpp \
                    tl-extensible-graph.cpp \
                    tl-pcfg-visitor.hpp \
                    tl-pcfg-visitor.cpp \
                    tl-dot-graph.cpp \
                    tl-structures.hpp \
                    $(END)
