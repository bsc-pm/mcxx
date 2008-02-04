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


lib_LTLIBRARIES = \
	libtlsuperscalar-configurator.la \
	libtlsuperscalar-preanalysis.la \
	libtlsuperscalar-task-analysis.la \
	libtlsuperscalar-function-router.la \
	libtlsuperscalar-transform-directives.la \
	libtlsuperscalar-code-conversion.la	\
	libtlsuperscalar-task-manifest.la


AM_CXXFLAGS = -Wall \
	-I $(top_srcdir)/src/tl \
	-I $(top_srcdir)/src/frontend \
	-I $(top_srcdir)/src/driver \
	-I $(top_srcdir)/lib \
	-I $(top_builddir)/src/tl \
	-I $(top_builddir)/src/frontend \
	-I $(top_builddir)/src/driver \
	-I $(top_builddir)/lib


libtlsuperscalar_configurator_la_SOURCES = \
	tl-configurator.cpp

libtlsuperscalar_preanalysis_la_SOURCES = \
	tl-preanalysis.cpp

libtlsuperscalar_task_analysis_la_SOURCES = \
	tl-source-bits.cpp \
	tl-task-analysis.cpp

libtlsuperscalar_function_router_la_SOURCES = \
	tl-function-router.cpp

libtlsuperscalar_transform_directives_la_SOURCES = \
	tl-transform-directives.cpp

libtlsuperscalar_code_conversion_la_SOURCES = 
if USE_TL_SUPERSCALAR_V1
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-code-conversion.cpp
endif
if USE_TL_SUPERSCALAR_V2
libtlsuperscalar_code_conversion_la_SOURCES += \
	tl-code-conversion-v2.cpp
endif

libtlsuperscalar_task_manifest_la_SOURCES = \
	tl-task-manifest.cpp


noinst_HEADERS = \
	tl-ast-predicates.hpp \
	tl-exceptions.hpp \
	tl-function-data.hpp \
	tl-source-bits.hpp \
	tl-type-utils.hpp \
	\
	tl-configurator.hpp \
	tl-preanalysis.hpp \
	tl-task-analysis.hpp \
	tl-function-router.hpp \
	tl-transform-directives.hpp \
	tl-code-conversion.hpp \
	tl-code-conversion-v2.hpp \
	tl-task-manifest.hpp

AM_LDFLAGS = -avoid-version
#-module -avoid-version

