/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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

#include "cxx-placeholders.h"
#include "cxx-process.h"
#include "cxx-ast.h"
#include "cxx-nodecl-output.h"

#include <string.h>
#include <stdio.h>

#define TOK_SEPARATOR "::"
static AST* decode_placeholder(const char *c)
{
    const char * colons = strstr(c, TOK_SEPARATOR);

    if (colons == NULL)
    {
        internal_error("Invalid placeholder token", 0);
    }

    colons += strlen(TOK_SEPARATOR);

    AST *tree = NULL;
    sscanf(colons, "%p", &tree);

    if (tree == NULL)
    {
        internal_error("Invalid AST* reference", 0);
    }

    return tree;
}

void check_statement_placeholder(AST placeholder, const decl_context_t* decl_context UNUSED_PARAMETER, nodecl_t* nodecl_output)
{
    AST* p = decode_placeholder(ASTText(placeholder));

    nodecl_t empty_stmt = nodecl_make_empty_statement(ast_get_locus(placeholder));

    nodecl_set_placeholder(empty_stmt, p);

    *p = nodecl_get_ast(empty_stmt);

    *nodecl_output = nodecl_make_list_1(_nodecl_wrap(*p));
}
