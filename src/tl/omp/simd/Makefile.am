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

include $(top_srcdir)/src/tl/Makefile.phases

phases_LTLIBRARIES = libtlomp-simd.la

END=

libtlomp_simd_la_CFLAGS = $(phases_cflags) \
                          -I $(srcdir)/../../vectorizer \
                          -I $(srcdir)/../../analysis/singleton \
                          -I $(top_srcdir)/src/tl/analysis/common \
                          -I $(top_srcdir)/src/tl/analysis/pcfg \
                          $(END)

libtlomp_simd_la_CXXFLAGS = $(phases_cflags) \
                          -I $(srcdir)/../../vectorizer \
                          -I $(srcdir)/../../analysis/singleton \
                          -I $(top_srcdir)/src/tl/analysis/common \
                          -I $(top_srcdir)/src/tl/analysis/pcfg \
                          $(END)

libtlomp_simd_la_LDFLAGS = $(phases_ldflags)
libtlomp_simd_la_LIBADD = $(phases_libadd) ../../vectorizer/libtlvectorizer.la


libtlomp_simd_la_SOURCES = \
   tl-omp-simd.hpp \
   tl-omp-simd.cpp \
   $(END)

