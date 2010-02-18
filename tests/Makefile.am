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

DIRTESTS = \
  mcxx.parser \
  mcc.typecalc \
  mcxx.typecalc \
  mcxx.torture2 \
  mcxx.cxx0x \
  mcc.extensions \
  mcxx.extensions \
  mcxx.gxx_type_traits \
  $(ADDITIONAL_TESTS) \
  $(NULL)

END=
CLEANFILES=

# Replacements are defined in this makefile
include ../config/Makefile.replace

check-local: $(builddir)/config/bets
	chmod +x config/mercurium-extensions
	chmod +x config/mercurium-omp
	chmod +x config/mercurium-ss
	chmod +x config/mercurium-stm
	chmod +x */mercurium
	FAIL=0; \
	for test_dir in $(DIRTESTS); \
	do \
       (if [ ! -d $(builddir)/$${test_dir}.dg ]; \
       then \
	      TEST_GENERATOR=$(builddir)/config/mercurium $(builddir)/config/bets -o $${test_dir}.log ${srcdir}/$${test_dir}.dg; \
       else \
	      TEST_GENERATOR=$(builddir)/$${test_dir}.dg/mercurium $(builddir)/config/bets -o $${test_dir}.log ${srcdir}/$${test_dir}.dg; \
       fi) || FAIL=1; \
	done; exit $${FAIL}

dist-hook:
	-for i in $(srcdir)/*.dg; \
	do  \
	    DIR=$$(basename $$i); \
		mkdir -p $(distdir)/$${DIR} ; \
		[ -e $(srcdir)/$${DIR}/config.mcxx ] && cp $(srcdir)/$${DIR}/config.mcxx $(distdir)/$${DIR} ; \
		[ -e $(srcdir)/$${DIR}/mercurium.in ] && cp $(srcdir)/$${DIR}/mercurium.in $(distdir)/$${DIR} ; \
        find $(srcdir)/$${DIR} "(" -name "*.c" -o -name "*.cpp" -o -name "*.C" -o -name "*.cc" ")" -exec cp {} $(distdir)/$${DIR} ';' ; \
	done

distclean-local:
	rm -f lt-mcxx_success*.c
	rm -f lt-mcxx_failure*.c
	rm -f lt-mcxx_success*.cpp
	rm -f lt-mcxx_failure*.cpp
	rm -f *backtrace.txt
	rm -f stm_unhandled_functions*.log
