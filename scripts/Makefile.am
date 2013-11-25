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

EXTRA_DIST= \
			bison-2.3-rofi-patch \
			mcxx-rpm.spec \
			$(DEBIAN_EXTRA)
			$(END)

DEBIAN_EXTRA=\
              debian/compat \
              debian/source/format \
              debian/rules \
              debian/control \
              $(END)

LATEST_GXX=g++-4.8
GXX_FLAGS=-fabi-version=6 -mavx -maes -mpclmul -mxop

generate_sse.bin : builtins_ia32_sse.cpp
	$(LATEST_GXX) $(GXX_FLAGS) -o generate_sse.bin $(srcdir)/builtins_ia32_sse.cpp

generate_sse: generate_sse.bin
	./generate_sse.bin > $(top_srcdir)/src/frontend/cxx-gccbuiltins-sse.h

CLEANFILES =
CLEANFILES += generate_sse.bin


rpm:
	@echo "Generating RPM structure"
	mkdir -p SOURCES BUILD RPMS SPECS SRPMS
	cp $(top_build_prefix)*.tar.gz SOURCES

	THREADS=$$(getconf _NPROCESSORS_ONLN); THREADS=$${THREADS:-1}; rpmbuild -v -bb --clean $(srcdir)/mcxx-rpm.spec --define '_topdir $(CURDIR)' --define 'version $(VERSION)' --define 'release $(shell date +%Y%m%d)' --define '_rpm_prefix $(RPM_PREFIX)' --define "threads $${THREADS}"

clean-rpm:
	-rm -rf BUILDROOT SOURCES BUILD RPMS SPECS SRPMS

deb:
	cp $(top_build_prefix)*.tar.gz mcxx_$(VERSION).orig.tar.gz
	tar -xf mcxx_$(VERSION).orig.tar.gz
	-rm -rf upstream_dir
	mv mcxx-$(VERSION) upstream_dir
	cp -rf $(srcdir)/debian upstream_dir/
	cp -rf upstream_dir/COPYING upstream_dir/debian/copyright
	-rm -f upstream_dir/debian/changelog

# Generate an empty changelog if git is not present
	if test ! -d $(top_srcdir)/.git; \
	then dch --create -c upstream_dir/debian/changelog --empty --package mcxx -v "$(VERSION)-$(shell date +%Y%m%d)$(DEB_RELEASE)"; \
	else git --git-dir="$(top_srcdir)/.git" --work-tree="$(top_srcdir)/.git" log -1 --pretty=format:"mcxx ($(VERSION)-$(shell date +%Y%m%d)$(DEB_RELEASE)) unstable; urgency=low%x0A%x0A  * %h %s%x0A%x0A -- %an <%ae>  %aD" > upstream_dir/debian/changelog; \
	fi

	THREADS=$$(getconf _NPROCESSORS_ONLN); THREADS=$${THREADS:-1}; cd upstream_dir; DEB_BUILD_OPTIONS="nocheck parallel=$${THREADS}" debuild -us -uc
	-rm -rf upstream_dir

clean-deb:
	-rm -f mcxx_$(VERSION).orig.tar.gz mcxx_$(VERSION)*.debian.tar.gz mcxx_$(VERSION)*.dsc mcxx_$(DEB_RELEASE)*.build mcxx_$(DEB_RELEASE)*.changes *.deb
	-rm -rf upstream_dir

clean-local: clean-rpm clean-deb
