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
			  mpi.mf03 \
			  $(END)

EXTRA_DIST = \
    attr_fn-f90-interfaces.h \
    conversion_fn_null-f90-interface.h \
    mpi.F90 \
    mpi-f90-interfaces.h \
    mpif-common.h \
    mpif-config.h \
    mpif-mpi-io.h \
    $(END)

CLEANFILES =
CLEANFILES += $(mpi_modules)

PLAINFC=$(top_builddir)/src/driver/mcxx --config-dir=$(top_builddir)/config --profile=plainfc --do-not-warn-config --do-not-wrap-modules

configdir=$(pkgdatadir)/fortran/mpi/openmpi
config_DATA = \
			  $(mpi_modules) \
			  $(END)

if SUPPORTED_SILENT_RULES
PLAINFC_verbose = $(PLAINFC_verbose_$(V))
PLAINFC_verbose_ = $(PLAINFC_verbose_$(AM_DEFAULT_VERBOSITY))
PLAINFC_verbose_0 = @echo "  PLAINFC" $@;
endif

mpi.mf03 : $(srcdir)/attr_fn-f90-interfaces.h \
	      $(srcdir)/conversion_fn_null-f90-interface.h \
		  $(srcdir)/mpi.F90 \
		  $(srcdir)/mpi-f90-interfaces.h \
		  $(srcdir)/mpif-common.h \
		  $(srcdir)/mpif-config.h \
		  $(srcdir)/mpif-mpi-io.h
	$(PLAINFC_verbose)$(PLAINFC) -y -o /dev/null $(srcdir)/mpi.F90
