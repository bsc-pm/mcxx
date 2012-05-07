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

info_TEXINFOS=mcxx-internals.texi mcxx-tl.texi

doxygen : doxygen_tl
	@echo "-- Running doxygen"
	[ -e doxygen_tl ] || ( echo "File 'doxygen_tl' is missing"; exit 1; )
	doxygen doxygen_tl

doxygen-trac: doxygen_tl
	cp -f doxygen_tl doxygen_trac
	echo "<!-- empty html -->" > empty.html
	$(SED) -i -e 's@^\s*\(HTML_OUTPUT\s*=\).*$$@\1 html_trac@' doxygen_trac
	$(SED) -i -e 's@^\s*\(HTML_HEADER\s*=\).*$$@\1 $(PWD)/empty.html@' doxygen_trac
	$(SED) -i -e 's@^\s*\(HTML_FOOTER\s*=\).*$$@\1 $(PWD)/empty.html@' doxygen_trac
	doxygen doxygen_trac

CLEANFILES=
CLEANFILES+=empty.html
CLEANFILES+=doxygen_trac

doxygen-pdf : doxygen
	cd latex && make all

clean-local:
	-rm -fr html latex
