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

#include "cxx-pragma.h"
#include "cxx-nodecl-output.h"
#include "cxx-ast.h"
#include "cxx-tltype.h"
#include "cxx-scope.h"
#include "string_utils.h"
#include <stdio.h>
        

static void common_build_scope_pragma_custom_clause_argument(AST a, 
        const decl_context_t* decl_context UNUSED_PARAMETER,
        nodecl_t *nodecl_output)
{
    *nodecl_output = nodecl_make_pragma_clause_arg(ASTText(a), ast_get_locus(a));
}

static void common_build_scope_pragma_custom_clause(AST a, const decl_context_t* decl_context, nodecl_t* nodecl_output)
{
    nodecl_t nodecl_argument = nodecl_null();
    if (ASTSon0(a) != NULL)
    {
        common_build_scope_pragma_custom_clause_argument(ASTSon0(a), decl_context, &nodecl_argument);
        // This is a list because it may be extended in later phases
        nodecl_argument = nodecl_make_list_1(nodecl_argument);
    }

    *nodecl_output = nodecl_make_pragma_custom_clause(nodecl_argument, strtolower(ASTText(a)), ast_get_locus(a));
}

void common_build_scope_pragma_custom_line(
        AST start_clauses, 
        AST end_clauses,
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_clauses = nodecl_null();
    nodecl_t nodecl_parameter = nodecl_null();

    if (start_clauses != NULL)
    {
        AST list, iter;
        list = ASTSon0(start_clauses);

        if (list != NULL)
        {
            for_each_element(list, iter)
            {
                AST pragma_clause = ASTSon1(iter);

                nodecl_t nodecl_clause = nodecl_null();
                common_build_scope_pragma_custom_clause(pragma_clause, decl_context, &nodecl_clause);

                nodecl_clauses = nodecl_append_to_list(nodecl_clauses, nodecl_clause);
            }
        }

        // #pragma XXX(parameter)
        AST parameter = ASTSon1(start_clauses);

        if (parameter != NULL)
        {
            common_build_scope_pragma_custom_clause_argument(parameter, decl_context, &nodecl_parameter);
            nodecl_parameter = nodecl_make_list_1(nodecl_parameter);
        }
    }

    nodecl_t nodecl_end_clauses = nodecl_null();
    if(end_clauses != NULL) 
    {    
        AST list, iter;
        list = ASTSon0(end_clauses);
        if(list != NULL)
        {
            for_each_element(list, iter)
            {
                AST pragma_clause = ASTSon1(iter);

                nodecl_t nodecl_end_clause = nodecl_null();
                common_build_scope_pragma_custom_clause(pragma_clause, decl_context, &nodecl_end_clause);
                nodecl_end_clauses = nodecl_append_to_list(nodecl_end_clauses, nodecl_end_clause);
            }
        }
    }

    *nodecl_output = nodecl_make_pragma_custom_line(nodecl_parameter, nodecl_clauses, nodecl_end_clauses, 
            strtolower(ASTText(start_clauses)), 
            ast_get_locus(start_clauses));
}

// Currently only used by Fortran
void common_build_scope_pragma_custom_statement(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output,
        nodecl_t* nodecl_pragma_line,
        void (*function_for_child)(AST, const decl_context_t* decl_context, nodecl_t*, void* info),
        void* info)
{
    common_build_scope_pragma_custom_line(ASTSon0(a), ASTSon2(a), decl_context, nodecl_pragma_line);

    nodecl_t nodecl_child = nodecl_null();
    function_for_child(ASTSon1(a), decl_context, &nodecl_child, info);

    *nodecl_output = nodecl_make_pragma_custom_statement(*nodecl_pragma_line, nodecl_child, strtolower(ASTText(a)), ast_get_locus(a));
}

void common_build_scope_pragma_custom_directive(AST a, 
        const decl_context_t* decl_context, 
        nodecl_t* nodecl_output)
{
    nodecl_t nodecl_pragma_line = nodecl_null();
    common_build_scope_pragma_custom_line(ASTSon0(a), /* end clauses */ NULL, decl_context, &nodecl_pragma_line);

    nodecl_t nodecl_pragma_context = nodecl_make_pragma_context(decl_context, ast_get_locus(a));
    *nodecl_output = nodecl_make_pragma_custom_directive(nodecl_pragma_line, nodecl_pragma_context, strtolower(ASTText(a)), ast_get_locus(a));
}
