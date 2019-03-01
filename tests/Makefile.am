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
EXTRA_DIST=

prepare-check: ./config/bets wipe-modules
	rm -f config/libraries-libtool.cache

wipe-modules:
	rm -f *.mod
	rm -f *.mf03

BETS_DIRS=$(srcdir)
LIT_DIR=$(top_srcdir)/scripts/lit
LIT_OPTIONS=-sv --threads=$(shell nproc) --param build-dir=$(PWD) --xunit-xml-output=test.log.xml
LIT=PYTHONPATH=$(LIT_SUPPORT) $(PYTHON) $(LIT_DIR)/lit.py $(LIT_OPTIONS)
LIT_SUPPORT=$(srcdir)

do-check : prepare-check
	$(MAKE) -C config
	$(MAKE) -C .
	$(LIT) $(BETS_DIRS)

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
			 $(BETS_DIRS)/07_phases_ompss_2.dg/fortran \
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
		$(BETS_DIRS)/07_phases_hlt.dg \
		$(END)

OMP_DIRS= \
        $(BETS_DIRS)/07_phases_omp.dg \
        $(OMPSS_DIRS) \
        $(END)

OMP_FORTRAN_DIRS= \
        $(BETS_DIRS)/07_phases_omp.dg/common/fortran \
        $(OMPSS_FORTRAN_DIRS) \
        $(END)

OMPSS_DIRS=\
        $(BETS_DIRS)/07_phases_ompss.dg \
        $(BETS_DIRS)/07_phases_ompss_2.dg \
	$(END)

OMPSS_FORTRAN_DIRS=\
        $(BETS_DIRS)/07_phases_ompss.dg/fortran \
        $(BETS_DIRS)/07_phases_ompss_2.dg/fortran \
	$(END)

ANALYSIS_DIRS=\
	$(BETS_DIRS)/07_phases_analysis.dg \
	$(END)

fortran-check : prepare-check
	$(LIT) $(FORTRAN_DIRS)

core-check : prepare-check
	$(LIT) $(CORE_DIRS)

core-fortran-check : prepare-check
	$(LIT) $(CORE_DIRS) $(FORTRAN_DIRS)

omp-check : prepare-check
	$(LIT) $(OMP_DIRS)

omp-fortran-check: prepare-check
	$(LIT) $(OMP_FORTRAN_DIRS)

ompss-check : prepare-check
	$(LIT) $(OMPSS_DIRS)

ompss-fortran-check : prepare-check
	$(LIT) $(OMPSS_FORTRAN_DIRS)

analysis-check : prepare-check
	$(LIT) $(ANALYSIS_DIRS)

LIT_FILES=\
	lit.site.cfg \
	litsupport/bets.py \
	litsupport/__init__.py

EXTRA_DIST+=$(LIT_FILES)

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

# Old bets targets, kept only just in case we need them
# TODO: Remove these at some point

BETS_OPTIONS="-only-fails"

bets-do-check : prepare-check
	$(MAKE) -C config
	$(MAKE) -C .
	export MALLOC_CHECK_=2; \
	./config/bets $(BETS_OPTIONS) $(BETS_DIRS)

bets-fortran-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(FORTRAN_DIRS)

bets-core-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(CORE_DIRS)

bets-core-fortran-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(CORE_DIRS) $(FORTRAN_DIRS)

bets-omp-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(OMP_DIRS)

bets-omp-fortran-check: prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(OMP_FORTRAN_DIRS)

bets-ompss-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(OMPSS_DIRS)

bets-ompss-fortran-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(OMPSS_FORTRAN_DIRS)

bets-analysis-check : prepare-check
	export MALLOC_CHECK_=2; \
	./config/bets \
		$(BETS_OPTIONS) $(ANALYSIS_DIRS)
