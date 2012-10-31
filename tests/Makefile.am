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

SUBDIRS = config .

NULL=

ADDITIONAL_TESTS=@ADDITIONAL_TESTSUITES@

END=
CLEANFILES=

CLEANFILES+=test.log test.log.xml

# Replacements are defined in this makefile
include ../config/Makefile.replace

prepare-check: ./config/bets
	chmod +x config/bets
	chmod +x config/mercurium-libraries
	chmod +x config/mercurium-extensions
	chmod +x config/mercurium-fortran
	chmod +x config/mercurium-nanox
	chmod +x config/mercurium-omp
	chmod +x config/mercurium-ompss
	chmod +x config/mercurium-simd
	chmod +x config/mercurium-cuda
	chmod +x config/mercurium-run
#	chmod +x config/mercurium-analysis
	chmod +x */mercurium

BETS_DIRS=$(srcdir)

do-check : prepare-check
	$(MAKE) -C config
	$(MAKE) -C .
	./config/bets $(BETS_OPTIONS) $(BETS_DIRS)

preserve-check-log:
if RUNNING_DISTCHECK
	( \
	preservedir="${top_builddir}/../../";  \
	echo "Trying to copy test.log.xml to $${preservedir}" ; \
	cp -vf test.log.xml $${preservedir} || true ; \
	)
endif

check-local : do-check preserve-check-log

FORTRAN_DIRS=\
			 $(BETS_DIRS)/01_fortran.dg \
			 $(BETS_DIRS)/07_phases_omp.dg/common/fortran \
			 $(BETS_DIRS)/07_phases_ompss.dg/fortran \
			 $(END)

CORE_DIRS=\
		$(BETS_DIRS)/01_parser.dg \
		$(BETS_DIRS)/02_typecalc_c.dg \
		$(BETS_DIRS)/02_typecalc_cxx.dg \
		$(BETS_DIRS)/03_extensions_c.dg \
		$(BETS_DIRS)/03_extensions_cxx.dg \
		$(BETS_DIRS)/04_compat_cxx0x.dg \
		$(BETS_DIRS)/04_compat_gcc.dg \
		$(BETS_DIRS)/04_compat_gxx.dg \
		$(BETS_DIRS)/04_compat_xl.dg \
		$(BETS_DIRS)/05_torture_cxx_1.dg \
		$(BETS_DIRS)/05_torture_cxx_2.dg \
		$(BETS_DIRS)/06_run_cxx.dg \
		$(END)

fortran-check : prepare-check
	./config/bets \
		$(BETS_OPTIONS) $(FORTRAN_DIRS)

core-check : prepare-check
	./config/bets \
		$(BETS_OPTIONS) $(CORE_DIRS)

core-fortran-check : prepare-check
	./config/bets \
		$(BETS_OPTIONS) $(CORE_DIRS) $(FORTRAN_DIRS)

dist-hook:
	for i in $(srcdir)/*.dg; \
	do  \
	    DIR=$$(basename $$i); \
        cp -vr $(srcdir)/$${DIR} $(distdir); \
	done

clean-local:
	rm -f lt-mcxx_success*.c
	rm -f lt-mcxx_failure*.c
	rm -f lt-mcxx_success*.cpp
	rm -f lt-mcxx_failure*.cpp
	rm -f *backtrace.txt
	rm -f stm_unhandled_functions*.log
	rm -f mcc_*.c mcc_*.f90.c mcc_*.f90.o
	rm -f mfc_*.f90 mfc_*.f90.c mfc_*.f90.o
	rm -f aux_*.f90.c aux_*.f90.o
	rm -f *.mod
