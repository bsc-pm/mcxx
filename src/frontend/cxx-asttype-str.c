/*--------------------------------------------------------------------
  (C) Copyright 2006-2015 Barcelona Supercomputing Center
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



#include "libmcxx-common.h"
#include "cxx-macros.h"
#include "cxx-asttype-str.h"
#include "cxx-asttype.h"
#include "cxx-process.h"
#include <string.h>

extern struct node_str_t * ast_node_name_to_kind_ (register const char *str, register unsigned int len);

#include "cxx-asttype-str-internal.h"

node_t ast_node_name_to_kind(const char* name)
{
	ERROR_CONDITION(name == NULL, "Invalid name", 0);
	node_str_t* n = ast_node_name_to_kind_(name, strlen(name));

	if (n == NULL)
		return AST_INVALID_NODE;
	else
		return n->kind;
}
