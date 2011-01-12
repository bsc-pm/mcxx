##########################################################################
#  (C) Copyright 2006-2011 Barcelona Supercomputing Center               #
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

include ../Makefile.phases

AM_CXXFLAGS = -Wall $(tl_cxxflags)

AM_CFLAGS = -Wall $(tl_cxxflags)

phases_LTLIBRARIES = \
	libtlsuperscalar-configurator.la \
	libtlsuperscalar-preanalysis.la \
	libtlsuperscalar-task-analysis.la \
	libtlsuperscalar-function-router.la \
	libtlsuperscalar-transform-directives.la \
	libtlsuperscalar-code-conversion.la	\
	libtlsuperscalar-task-reshaper.la \
	libtlsuperscalar-task-manifest.la


if SUPPORTED_SILENT_RULES
BISON_verbose = $(BISON_verbose_$(V))
BISON_verbose_ = $(BISON_verbose_$(AM_DEFAULT_VERBOSITY))
BISON_verbose_0 = @echo "  BISON " $@;
endif

if BISON_BUILDING
CLEANFILES = \
	calculator-parser.c
endif

EXTRA_DIST = calculator-parser.y

calculator-parser.c calculator-parser.output : calculator-parser.y
if BISON_BUILDING
	$(BISON_verbose)$(BISON) --output=calculator-parser.c $<
else
	@echo "*** ERROR: file" $< "was modified but no suitable bison-rofi was found during configure ***" && /bin/false
endif


libtlsuperscalar_configurator_la_SOURCES = \
	tl-configurator.cpp

libtlsuperscalar_preanalysis_la_SOURCES = \
	tl-function-table.cpp \
	tl-preanalysis.cpp

libtlsuperscalar_task_analysis_la_SOURCES = \
	calculator-parser.c \
	tl-calculator.cpp \
	tl-source-bits.cpp \
	tl-parameter-expression.cpp \
	tl-region.cpp \
	tl-task-analysis.cpp

libtlsuperscalar_function_router_la_SOURCES = \
	tl-function-table.cpp \
	tl-function-router.cpp

libtlsuperscalar_transform_directives_la_SOURCES = 
if USE_TL_SUPERSCALAR_V3
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives-v3.cpp
endif
if USE_TL_SUPERSCALAR_V4
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives-v3.cpp
endif
if USE_TL_SUPERSCALAR_V5
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives-v3.cpp
endif

libtlsuperscalar_code_conversion_la_SOURCES = 
if USE_TL_SUPERSCALAR_V3
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-parameter-expression.cpp \
	tl-task-table.cpp \
	tl-code-conversion-v3.cpp
endif
if USE_TL_SUPERSCALAR_V4
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-parameter-expression.cpp \
	tl-task-table.cpp \
	tl-code-conversion-v4.cpp
endif
if USE_TL_SUPERSCALAR_V5
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-parameter-expression.cpp \
	tl-task-table.cpp \
	tl-code-conversion-v5.cpp
endif

libtlsuperscalar_task_reshaper_la_SOURCES = \
	tl-task-table.cpp \
	tl-task-reshaper.cpp
	
libtlsuperscalar_task_manifest_la_SOURCES = \
	tl-task-table.cpp \
	tl-task-manifest.cpp


noinst_HEADERS = \
	cxx-ss-attrnames.h \
	calculator-parser-types.h\
	\
	tl-access-bounds.hpp \
	tl-access-bounds-list.hpp \
	tl-ast-predicates.hpp \
	tl-augmented-symbol.hpp \
	tl-exceptions.hpp \
	tl-parameter-expression.hpp \
	tl-parameter-region-list.hpp \
	tl-region.hpp \
	tl-region-list.hpp \
	tl-source-bits.hpp \
	tl-task-table.hpp \
	tl-type-utils.hpp \
	\
	tl-function-table.hpp \
	tl-calculator.hpp \
	tl-configurator.hpp \
	tl-preanalysis.hpp \
	tl-task-analysis.hpp \
	tl-function-router.hpp \
	tl-transform-directives.hpp \
	tl-transform-directives-v3.hpp \
	tl-code-conversion-v3.hpp \
	tl-code-conversion-v4.hpp \
	tl-code-conversion-v5.hpp \
	tl-task-reshaper.hpp \
	tl-task-manifest.hpp

AM_LDFLAGS = $(tl_ldflags) -lm
#-module -avoid-version

