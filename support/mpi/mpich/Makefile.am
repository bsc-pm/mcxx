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


mpi_modules = \
			  mpi.mod \
			  $(END)

EXTRA_DIST = \
    mpi_base.f90 \
    mpi_constants.f90 \
    mpi.f90 \
    mpifnoext.h \
    mpi_sizeofs.f90 \
    $(END)

CLEANFILES =
CLEANFILES += $(mpi_modules) mpi_base.mod mpi_constants.mod mpi_sizeofs.mod mpi.o mpi_constants.o mpi_base.o mpi_sizeofs.o

PLAINFC=$(top_builddir)/src/driver/mcxx --config-dir=$(top_builddir)/config --profile=plainfc --do-not-warn-config

configdir=$(pkgdatadir)/fortran/mpi/mpich
config_DATA = \
			  $(mpi_modules) \
			  $(END)

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

mpi.mod : mpi_constants.mod mpi_sizeofs.mod mpi_base.mod
	$(PLAINFC_verbose)$(PLAINFC) -c $(srcdir)/mpi.f90

mpi_constants.mod : $(srcdir)/mpi_constants.f90 $(srcdir)/mpifnoext.h
	$(PLAINFC_verbose)$(PLAINFC) -c $(srcdir)/mpi_constants.f90

mpi_sizeofs.mod : $(srcdir)/mpi_sizeofs.f90
	$(PLAINFC_verbose)$(PLAINFC) -c $(srcdir)/mpi_sizeofs.f90

mpi_base.mod : mpi_constants.mod $(srcdir)/mpi_base.f90
	$(PLAINFC_verbose)$(PLAINFC) -c $(srcdir)/mpi_base.f90
