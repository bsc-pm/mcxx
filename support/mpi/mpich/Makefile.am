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


mpi_modules = \
			  mpi.mf03 \
			  $(END)

EXTRA_DIST = \
    mpi_base.f90 \
    mpi_constants.f90 \
    mpi.f90 \
    mpifnoext.h \
    mpi_sizeofs.f90 \
    $(END)

CLEANFILES =
CLEANFILES += $(mpi_modules) mpi_base.mf03 mpi_constants.mf03 mpi_sizeofs.mf03

PLAINFC_PROFILE=`echo plainfc | $(SED) '$(program_transform_name)'`

PLAINFC=$(top_builddir)/src/driver/mcxx --config-dir=$(top_builddir)/config --profile=$(PLAINFC_PROFILE) --do-not-warn-config --do-not-wrap-modules

configdir=$(pkgdatadir)/fortran/mpi/mpich
config_DATA = \
			  $(mpi_modules) \
			  $(END)

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

mpi.mf03 : mpi_constants.mf03 mpi_sizeofs.mf03 mpi_base.mf03
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/mpi.f90

mpi_constants.mf03 : $(srcdir)/mpi_constants.f90 $(srcdir)/mpifnoext.h
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/mpi_constants.f90

mpi_sizeofs.mf03 : $(srcdir)/mpi_sizeofs.f90
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/mpi_sizeofs.f90

mpi_base.mf03 : mpi_constants.mf03 $(srcdir)/mpi_base.f90
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/mpi_base.f90
