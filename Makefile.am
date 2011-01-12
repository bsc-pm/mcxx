##########################################################################
#  (C) Copyright 2006-2011 Barcelona Supercomputing Center               #
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

SUBDIRS = gnulib lib src config tests scripts

DIST_SUBDIRS = $(SUBDIRS) doc

ACLOCAL_AMFLAGS=-I m4

SUPERSCALAR_DISTCHECK_CONFIGURE_FLAGS = --with-superscalar-runtime-api-version=@superscalar_version@

DISTCHECK_CONFIGURE_FLAGS = $(SUPERSCALAR_DISTCHECK_CONFIGURE_FLAGS)

dist-hook:
	if [ -n "$(gitpath)" ]; \
	then \
	    run_gitversion=`"$(gitpath)" --git-dir=$(srcdir)/.git show "--pretty=format:%h %ci" HEAD | head -n 1`; \
	    build_version="git $${run_gitversion} distributed version"; \
	    echo "$${build_version}" > $(distdir)/DIST_REVISION; \
	elif [ -e $(srcdir)/DIST_REVISION ]; \
	then \
	    cp $(srcdir)/DIST_REVISION $(distdir)/DIST_REVISION; \
	fi
