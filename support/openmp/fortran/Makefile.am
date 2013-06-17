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

omp_modules = \
    omp_lib_kinds.mf03 \
    omp_lib.mf03 \
    $(END)

EXTRA_DIST=\
		   omp_modules.f90 \
		   omp_lib.h \
		   $(END)

CLEANFILES =
CLEANFILES += $(omp_modules)

PLAINFC_PROFILE=`echo plainfc | $(SED) '$(program_transform_name)'`

PLAINFC=$(top_builddir)/src/driver/mcxx --config-dir=$(top_builddir)/config --profile=$(PLAINFC_PROFILE) --do-not-warn-config --do-not-wrap-modules

configdir=$(pkgdatadir)/fortran/openmp
config_DATA = \
			  $(omp_modules) \
			  omp_lib.h \
			  $(END)

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

# This is a noop to avoid breaking parallel builds
omp_lib_kinds.mf03 : omp_lib.mf03

omp_lib.mf03 : $(srcdir)/omp_modules.f90
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/omp_modules.f90
