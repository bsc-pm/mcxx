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

#include <stdlib.h>
#include "mem.h"

void *counted_xmalloc(size_t nmemb, size_t size, unsigned long long *counter)
{
    if (size == 0
            || nmemb == 0)
    {
        // Ensure a NULL when we allocate 0 bytes
        return NULL;
    }

    if (counter != NULL)
    {
        (*counter) += (size * nmemb);
    }

    return xmalloc(nmemb * size);
}

void *counted_xcalloc(size_t nmemb, size_t size, unsigned long long *counter)
{
    if (size == 0
            || nmemb == 0)
    {
        // Ensure a NULL when we allocate 0 bytes
        return NULL;
    }

    if (counter != NULL)
    {
        (*counter) += (size * nmemb);
    }

    return xcalloc(nmemb, size);
}
