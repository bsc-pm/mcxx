#	Cell Superscalar (CellSs): Easy programming the Cell BE
#	Copyright (C) 2007 Barcelona Supercomputing Center - Centro Nacional de Supercomputacion
#	
#	This program is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License
#	as published by the Free Software Foundation; version 2.
#	
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#	
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. 
#	
#	The GNU General Public License is contained in the file COPYING.


# Ugly, but necessary since normal YACC rules have been broken
BISON = @BISON@

include ../Makefile.phases

AM_CXXFLAGS = -Wall $(tl_cxxflags)

AM_CFLAGS = -Wall $(tl_cxxflags)

lib_LTLIBRARIES = \
	libtlsuperscalar-configurator.la \
	libtlsuperscalar-preanalysis.la \
	libtlsuperscalar-task-analysis.la \
	libtlsuperscalar-function-router.la \
	libtlsuperscalar-transform-directives.la \
	libtlsuperscalar-code-conversion.la	\
	libtlsuperscalar-task-manifest.la


if BISON_BUILDING
CLEANFILES = \
	calculator-parser.c
endif

EXTRA_DIST = calculator-parser.y

calculator-parser.c calculator-parser.output : calculator-parser.y
if BISON_BUILDING
	$(BISON) --output=calculator-parser.c $<
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
if USE_TL_SUPERSCALAR_V1
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives.cpp
endif
if USE_TL_SUPERSCALAR_V2
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives.cpp
endif
if USE_TL_SUPERSCALAR_V3
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives-v3.cpp
endif
if USE_TL_SUPERSCALAR_V4
libtlsuperscalar_transform_directives_la_SOURCES += \
	tl-transform-directives-v3.cpp
endif

libtlsuperscalar_code_conversion_la_SOURCES = 
if USE_TL_SUPERSCALAR_V1
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-code-conversion.cpp
endif
if USE_TL_SUPERSCALAR_V2
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-code-conversion-v2.cpp
endif
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

libtlsuperscalar_task_manifest_la_SOURCES = \
	tl-task-table.cpp \
	tl-task-manifest.cpp


noinst_HEADERS = \
	cxx-ss-attrnames.h \
	calculator-parser-types.h\
	\
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
	tl-code-conversion.hpp \
	tl-code-conversion-v2.hpp \
	tl-code-conversion-v3.hpp \
	tl-code-conversion-v4.hpp \
	tl-parameter-region-list.hpp \
	tl-parameter-expression.hpp \
	tl-task-manifest.hpp

AM_LDFLAGS = $(tl_ldflags) -lm
#-module -avoid-version

