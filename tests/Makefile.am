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

SUBDIRS = config

NULL=

ADDITIONAL_TESTS=@ADDITIONAL_TESTSUITES@

#MERCURIUM_TESTS = \
#  mcxx.parser \
#  mcc.typecalc \
#  mcxx.typecalc \
#  mcxx.torture \
#  mcxx.cxx0x \
#  mcc.extensions \
#  mcxx.extensions \
#  mcxx.gxx_type_traits \
#  mcc.hlt \
#  $(ADDITIONAL_TESTS) \
#  $(NULL)

MERCURIUM_TESTS = \
  mcxx.parser \
  mcc.typecalc \
  mcxx.typecalc \
  mcxx.torture2 \
  mcxx.cxx0x \
  mcc.extensions \
  mcxx.extensions \
  mcxx.gxx_type_traits \
  $(NULL)

END=
CLEANFILES=

CONFIG_FILES_TESTS= \
                    mcxx.instr.nanos4.dg/config.mcxx \
                    mcxx.nanos4.dg/config.mcxx \
                    mcc.superscalar.dg/config.mcxx \
                    mcxx.stm.dg/config.mcxx \
                    mcc.instr.nanos4.dg/config.mcxx \
                    mcc.nanos4.dg/config.mcxx \
                    mcc.hlt.basic.dg/config.mcxx \
                    $(END)

CLEANFILES += $(CONFIG_FILES_TESTS)
BUILT_SOURCES=$(CONFIG_FILES_TESTS)

# Replacements are defined in this makefile
include ../config/Makefile.replace

check:
	chmod +x */mercurium
	for test_dir in $(MERCURIUM_TESTS); \
	do \
       if [ ! -d $(builddir)/$${test_dir}.dg ]; \
       then \
	      TEST_GENERATOR=$(builddir)/config/mercurium $(srcdir)/config/bets -o $${test_dir}.log ${srcdir}/$${test_dir}.dg; \
       else \
	      TEST_GENERATOR=$(builddir)/$${test_dir}.dg/mercurium $(srcdir)/config/bets -o $${test_dir}.log ${srcdir}/$${test_dir}.dg; \
       fi \
	done

$(CONFIG_FILES_TESTS) : Makefile
	$(config_edit) $(srcdir)/$@.in > $@.tmp
	mv $@.tmp $@

dist-hook:
	-cp $(srcdir)/*.conf $(distdir)
	-for i in $(srcdir)/*.dg $(srcdir)/*.files; \
	do  \
	    DIR=$$(basename $$i); \
		mkdir -p $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.exp $(distdir)/$${DIR} ; \
		[ "(" -e "$(srcdir)/$${DIR}/config.mcxx" ")" -a "(" ! -e  "$(srcdir)/$${DIR}/config.mcxx.in" ")" ] \
               && cp $(srcdir)/$${DIR}/config.mcxx $(distdir)/$${DIR}; \
		cp $(srcdir)/$${DIR}/config.mcxx.in $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/mercurium.in $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.c $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.cpp $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.C $(distdir)/$${DIR} ; \
		cp $(srcdir)/$${DIR}/*.cc $(distdir)/$${DIR} ; \
	done

distclean-local:
	rm -vf lt-mcxx_success*.c
	rm -vf lt-mcxx_failure*.c
	rm -vf lt-mcxx_success*.cpp
	rm -vf lt-mcxx_failure*.cpp
	rm -vf *backtrace.txt
	rm -vf stm_unhandled_functions*.log
