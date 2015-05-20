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


#include "cxx-locus.h"
#include <stdlib.h>
#include "string_utils.h"

struct locus_tag
{
    const char* filename;
    unsigned int line, col;
};

static inline const char* locus_to_str(const locus_t* l)
{
    const char* result = NULL;
    if (l == NULL)
        return ":0";

    if (l->col != 0)
        uniquestr_sprintf(&result, "%s:%d:%d", l->filename, l->line, l->col);
    else
        uniquestr_sprintf(&result, "%s:%d", l->filename, l->line);

    return result;
}

static inline const char* locus_get_filename(const locus_t* l)
{
    if (l == NULL)
        return "";
    return l->filename;
}

static inline unsigned int locus_get_line(const locus_t* l)
{
    return l == NULL ? 0 : l->line;
}

static inline unsigned int locus_get_column(const locus_t* l)
{
    return l == NULL ? 0 : l->col;
}
