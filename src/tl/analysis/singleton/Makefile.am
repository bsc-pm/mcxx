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

lib_LTLIBRARIES= libanalysis_singleton.la

END=

libanalysis_singleton_la_CFLAGS= $(tl_cflags)
libanalysis_singleton_la_CXXFLAGS= $(tl_cflags) \
				-I$(srcdir)/../.. \
				-I$(srcdir)/../common \
				-I$(srcdir)/../pcfg \
				-I$(srcdir)/../use_def \
				-I$(srcdir)/../liveness \
				-I$(srcdir)/../reach_defs \
				-I$(srcdir)/../loops \
				-I$(srcdir)/../auto-scope \
				-I$(srcdir)/../range \
				$(END)

libanalysis_singleton_la_LDFLAGS= $(tl_ldflags)
libanalysis_singleton_la_LIBADD= \
				$(tl_libadd) \
				../../libtl.la \
				../common/libanalysis_utils.la \
				../pcfg/libpcfg.la \
				../use_def/libuse_def.la \
				../liveness/libliveness.la \
				../reach_defs/libreaching_definitions.la \
				../loops/libloops_analysis.la \
				../auto-scope/libauto_scope.la \
				../range/librange.la \
				$(END)

libanalysis_singleton_la_SOURCES= tl-analysis-static-info.cpp \
                                  tl-analysis-static-info.hpp \
                                  tl-vectorization-analysis.cpp \
                                  tl-analysis-singleton.hpp \
                                  tl-analysis-singleton.cpp \
                                  $(END)
