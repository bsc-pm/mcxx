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

phases_LTLIBRARIES = libtlvectorizer.la

END=


libtlvectorizer_la_CFLAGS = $(phases_cflags) -Wall \
                            -I $(top_srcdir)/src/tl/analysis/singleton/ \
                            -I $(top_srcdir)/src/tl/analysis/common/ \
                            -I $(top_srcdir)/src/tl/analysis/pcfg/ \
                            $(END)

libtlvectorizer_la_CXXFLAGS = $(phases_cflags) -Wall \
                              -I $(top_srcdir)/src/tl/analysis/singleton/ \
                              -I $(top_srcdir)/src/tl/analysis/common/ \
                              -I $(top_srcdir)/src/tl/analysis/pcfg/ \
                              $(END)

libtlvectorizer_la_LDFLAGS = $(phases_ldflags)
libtlvectorizer_la_LIBADD = $(phases_libadd) \
                          $(top_builddir)/src/tl/analysis/singleton/libanalysis_singleton.la \
                          $(END)

libtlvectorizer_la_SOURCES = \
                           tl-vectorizer-utils.hpp \
                           tl-vectorizer-utils.cpp \
                           tl-function-versioning.cpp \
                           tl-function-versioning.hpp \
                           tl-vectorizer.cpp \
                           tl-vectorizer.hpp \
                           tl-vectorizer-visitor-for.cpp \
                           tl-vectorizer-visitor-for.hpp \
                           tl-vectorizer-visitor-function.cpp \
                           tl-vectorizer-visitor-function.hpp \
                           tl-vectorizer-visitor-statement.cpp \
                           tl-vectorizer-visitor-statement.hpp \
                           tl-vectorizer-visitor-expression.cpp \
                           tl-vectorizer-visitor-expression.hpp \
                           tl-vectorizer-vector-reduction.hpp \
                           tl-vectorizer-vector-reduction.cpp \
                           $(END)

