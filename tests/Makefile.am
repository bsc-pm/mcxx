##########################################################################
#  (C) Copyright 2006-2009 Barcelona Supercomputing Center               #
#                          Centro Nacional de Supercomputacion           #
#                                                                        #
#  This file is part of Mercurium C/C++ source-to-source compiler.       #
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

# Replacements are defined in this makefile
include ../config/Makefile.replace

prepare-check: $(builddir)/config/bets
	chmod +x config/mercurium-extensions
	chmod +x config/mercurium-omp
	chmod +x config/mercurium-nanos4
	chmod +x config/mercurium-nanox
	chmod +x config/mercurium-ss
	chmod +x config/mercurium-stm
	chmod +x */mercurium

BETS_DIRS=$(srcdir)

check-local: prepare-check
	$(builddir)/config/bets $(BETS_OPTIONS) $(BETS_DIRS)

dist-hook:
	for i in $(srcdir)/*.dg; \
	do  \
	    DIR=$$(basename $$i); \
        cp -r $(srcdir)/$${DIR} $(distdir)/$${DIR}; \
	done

distclean-local:
	rm -f lt-mcxx_success*.c
	rm -f lt-mcxx_failure*.c
	rm -f lt-mcxx_success*.cpp
	rm -f lt-mcxx_failure*.cpp
	rm -f *backtrace.txt
	rm -f stm_unhandled_functions*.log
