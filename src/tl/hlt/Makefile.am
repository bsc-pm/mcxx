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

lib_LTLIBRARIES = libtl-hlt.la
phases_LTLIBRARIES = libtl-hlt-pragma.la

include $(top_srcdir)/src/tl/Makefile.phases

END=

ANALYSIS_CFLAGS= \
			  -I $(srcdir)/../analysis/common \
			  -I $(srcdir)/../analysis/constants \
			  -I $(srcdir)/../analysis/liveness \
			  -I $(srcdir)/../analysis/loops \
			  -I $(srcdir)/../analysis/pcfg \
			  -I $(srcdir)/../analysis/reach_defs \
			  -I $(srcdir)/../analysis/singleton \
			  -I $(srcdir)/../analysis/test_phase \
			  -I $(srcdir)/../analysis/use_def \
			  $(END)

ANALYSIS_LDFLAGS = ../analysis/singleton/libanalysis_singleton.la
 
COMMON_CFLAGS=-DLIBHLT_DLL_EXPORT \
			  -I $(srcdir)/../omp/core \
			  $(ANALYSIS_CFLAGS) \
			  $(END)

libtl_hlt_la_CFLAGS=$(tl_cflags) $(COMMON_CFLAGS)
libtl_hlt_la_CXXFLAGS=$(tl_cxxflags) $(COMMON_CFLAGS)
libtl_hlt_la_LIBADD=$(tl_libadd)
libtl_hlt_la_LDFLAGS=$(tl_ldflags) $(ANALYSIS_LDFLAGS)

libtl_hlt_la_SOURCES = \
	  hlt-common.hpp \
      hlt-transform.hpp \
      hlt-transform.cpp \
      hlt-unroll.hpp \
      hlt-unroll.cpp \
      $(END)

libtl_hlt_pragma_la_CFLAGS=$(phases_cflags) $(COMMON_CFLAGS)
libtl_hlt_pragma_la_CXXFLAGS=$(phases_cxxflags) $(COMMON_CFLAGS)
libtl_hlt_pragma_la_LIBADD=$(phases_libadd) libtl-hlt.la
libtl_hlt_pragma_la_LDFLAGS=$(phases_ldflags)

libtl_hlt_pragma_la_SOURCES = \
      hlt-pragma.cpp \
      hlt-pragma.hpp \
      $(END)

