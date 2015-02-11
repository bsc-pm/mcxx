/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
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




#include "cxx-dyninit.h"
#include "cxx-utils.h"

static dynamic_initializer_t* dynamic_initializer_list = NULL;
static int already_initialized = 0;
static int num_dynamic_initializers = 0;

void register_dynamic_initializer(dynamic_initializer_t dynamic_initializer)
{
    P_LIST_ADD(dynamic_initializer_list, num_dynamic_initializers, dynamic_initializer);
}

void run_dynamic_initializers(void)
{
    if (already_initialized)
        return;

    int i;
    for (i = 0; i < num_dynamic_initializers; i++)
    {
        (dynamic_initializer_list[i])();
    }

    already_initialized = 1;
}
