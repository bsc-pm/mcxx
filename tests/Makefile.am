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

SUBDIRS = config .

END=
DISTCLEANFILES=

DISTCLEANFILES+=test.log test.log.xml

prepare-check: ./config/bets wipe-modules
	chmod +x config/bets
	chmod +x config/mercurium-libraries
	chmod +x config/mercurium-fe-only
	chmod +x config/mercurium-extensions
	chmod +x config/mercurium-fortran
	chmod +x config/mercurium-nanox
	chmod +x config/mercurium-nanos6
	chmod +x config/mercurium-hlt
	chmod +x config/mercurium-omp
	chmod +x config/mercurium-ompss
	chmod +x config/mercurium-serial-simd
	chmod +x config/mercurium-parallel-simd
	chmod +x config/mercurium-serial-simd-avx2
	chmod +x config/mercurium-parallel-simd-avx2
	chmod +x config/mercurium-serial-simd-mic
	chmod +x config/mercurium-parallel-simd-mic
	chmod +x config/mercurium-serial-simd-avx2
	chmod +x config/mercurium-parallel-simd-avx2
	chmod +x config/mercurium-serial-simd-romol
	chmod +x config/mercurium-cuda
	chmod +x config/mercurium-opencl
	chmod +x config/mercurium-run
	chmod +x config/mercurium-analysis
	chmod +x */mercurium
	chmod +x */mercurium-c11
	chmod +x */mercurium-cxx11
	chmod +x */mercurium-cxx14
	rm -f config/libraries-libtool.cache

wipe-modules:
	rm -f *.mod
	rm -f *.mf03

BETS_DIRS=$(srcdir)

BETS_OPTIONS="-only-fails"

do-check : prepare-check
	$(MAKE) -C config
	$(MAKE) -C .
	export MALLOC_CHECK_=2; \
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
			 $(BETS_DIRS)/03_extensions_f.dg \
			 $(BETS_DIRS)/07_phases_omp.dg/common/fortran \
			 $(BETS_DIRS)/07_phases_ompss.dg/fortran \
			 $(END)

CORE_DIRS=\
		$(BETS_DIRS)/01_parser.dg \
		$(BETS_DIRS)/02_typecalc_c.dg \
		$(BETS_DIRS)/02_typecalc_c11.dg \
		$(BETS_DIRS)/02_typecalc_cxx.dg \
		$(BETS_DIRS)/02_typecalc_cxx11.dg \
		$(BETS_DIRS)/02_typecalc_cxx14.dg \
		$(BETS_DIRS)/03_extensions_c.dg \
		$(BETS_DIRS)/03_extensions_cxx.dg \
		$(BETS_DIRS)/04_compat_cxx0x.dg \
		$(BETS_DIRS)/04_compat_gcc.dg \
		$(BETS_DIRS)/04_compat_gxx.dg \
		$(BETS_DIRS)/04_compat_xl.dg \
		$(BETS_DIRS)/05_torture_cxx_1.dg \
		$(BETS_DIRS)/05_torture_cxx_2.dg \
		$(BETS_DIRS)/06_run_cxx.dg \
		$(BETS_DIRS)/07_phases_hlt.dg \
		$(END)

OMP_DIRS= \
        $(BETS_DIRS)/07_phases_omp.dg \
        $(OMPSS_DIRS) \
        $(END)

OMPSS_DIRS=\
        $(BETS_DIRS)/07_phases_ompss.dg \
		$(END)

ANALYSIS_DIRS=\
	$(BETS_DIRS)/07_phases_analysis.dg \
	$(END)

fortran-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(FORTRAN_DIRS)

core-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(CORE_DIRS)

core-fortran-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(CORE_DIRS) $(FORTRAN_DIRS)

omp-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(OMP_DIRS)

ompss-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(OMPSS_DIRS)

analysis-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(ANALYSIS_DIRS)
		
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
	rm -f *.mf03

