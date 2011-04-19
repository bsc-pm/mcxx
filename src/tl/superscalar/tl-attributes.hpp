/*--------------------------------------------------------------------
  (C) Copyright 2006-2011 Barcelona Supercomputing Center 
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information 
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




#ifndef TL_ATTRIBUTES_H
#define TL_ATTRIBUTES_H


namespace TL {
	static char const * const SYMBOL_SUPERSCALAR_PARAMETER_REGION_LIST = "symbol.superscalar.parameter_region_list";
	static char const * const SYMBOL_SUPERSCALAR_PARAMETER_ACCESS_BOUNDS_LIST = "symbol.superscalar.parameter_access_bounds_list";
	static char const * const SYMBOL_SUPERSCALAR_IS_TASK = "symbol.superscalar.is_task";
	static char const * const SYMBOL_SUPERSCALAR_HAS_HIGH_PRIORITY = "symbol.superscalar.has_high_priority";
	static char const * const SYMBOL_SUPERSCALAR_HAS_COHERCED_SIDES = "symbol.superscalar.has_coherced_sides";
	static char const * const SYMBOL_SUPERSCALAR_IS_ON_TASK_SIDE = "symbol.superscalar.is_on_task_side";
	static char const * const SYMBOL_SUPERSCALAR_IS_ON_NON_TASK_SIDE = "symbol.superscalar.is_on_non_task_side";
	static char const * const SYMBOL_SUPERSCALAR_CALLS_TO_TASKSIDE_COHERCED_FUNCTION = "symbol.superscalar.calls_to_taskside_coherced_function";
	
}

#endif // TL_ATTRIBUTES_H
