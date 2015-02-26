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



#include "cxx-intelsupport.h"

#include "cxx-typeutils.h"
#include "cxx-nodecl.h"
#include "cxx-nodecl-output.h"
#include "cxx-exprtype.h"
#include "cxx-diagnostic.h"
#include "cxx-utils.h"

void intel_check_assume(
        AST expression,
        decl_context_t decl_context,
        nodecl_t* nodecl_out)
{
    AST assumed_expr = ast_get_child(expression, 0);

    nodecl_t nodecl_assumed_expr = nodecl_null();
    check_expression_non_executable(assumed_expr, decl_context, &nodecl_assumed_expr);

    intel_check_assume_nodecl(nodecl_assumed_expr,
            decl_context, 
            ast_get_locus(expression),
            nodecl_out);
}

void intel_check_assume_nodecl(
        nodecl_t assumed_expr,
        decl_context_t decl_context,
        const locus_t* locus,
        nodecl_t* nodecl_out)
{
    if (nodecl_is_err_expr(assumed_expr))
    {
        *nodecl_out = assumed_expr;
        return;
    }

    type_t* t = nodecl_get_type(assumed_expr);

    standard_conversion_t scs;
    C_LANGUAGE()
    {
        if (!standard_conversion_between_types(&scs, t, get_signed_int_type(), locus))
        {
            error_printf("%s: error: argument of '__assume' is of type '%s' not convertible to int\n",
                    locus_to_str(locus),
                    print_type_str(t, decl_context));
            nodecl_free(assumed_expr);
            *nodecl_out = nodecl_make_err_expr(locus);
            return;
        }
    }
    CXX_LANGUAGE()
    {
        if (!standard_conversion_between_types(&scs, t, get_bool_type(), locus))
        {
            error_printf("%s: error: argument of '__assume' is of type '%s' not convertible to bool\n",
                    nodecl_locus_to_str(assumed_expr),
                    print_type_str(t, decl_context));
            nodecl_free(assumed_expr);
            *nodecl_out = nodecl_make_err_expr(locus);
            return;
        }
    }

    *nodecl_out = nodecl_make_intel_assume(assumed_expr, get_void_type(), locus);
}

void intel_check_assume_aligned(
        AST expression,
        decl_context_t decl_context,
        nodecl_t* nodecl_out)
{
    AST pointer_expr = ast_get_child(expression, 0);

    nodecl_t nodecl_pointer_expr = nodecl_null();
    check_expression_non_executable(pointer_expr, decl_context, &nodecl_pointer_expr);

    AST alignment = ast_get_child(expression, 1);

    nodecl_t nodecl_alignment_expr = nodecl_null();
    check_expression_non_executable_must_be_constant(alignment, decl_context, &nodecl_alignment_expr);

    intel_check_assume_aligned_nodecl(
            nodecl_pointer_expr,
            nodecl_alignment_expr,
            decl_context,
            ast_get_locus(expression),
            nodecl_out);
}

void intel_check_assume_aligned_nodecl(
        nodecl_t pointer_arg,
        nodecl_t alignment,
        decl_context_t decl_context UNUSED_PARAMETER,
        const locus_t* locus,
        nodecl_t* nodecl_out)
{
    if (nodecl_is_err_expr(pointer_arg))
    {
        *nodecl_out = pointer_arg;
        nodecl_free(alignment);
        return;
    }

    if (nodecl_is_err_expr(alignment))
    {
        *nodecl_out = alignment;
        nodecl_free(pointer_arg);
        return;
    }

    if (!is_pointer_type(no_ref(nodecl_get_type(pointer_arg))))
    {
        error_printf("%s: error: first argument of __assume_aligned must be a pointer\n",
                nodecl_locus_to_str(pointer_arg));

        *nodecl_out = nodecl_make_err_expr(locus);
        nodecl_free(pointer_arg);
        nodecl_free(alignment);
        return;
    }

    if (!nodecl_is_constant(alignment) || !const_value_is_integer(nodecl_get_constant(alignment)))
    {
        error_printf("%s: error: second argument of __assume_aligned argument must be an integer constant\n",
                nodecl_locus_to_str(alignment));
        *nodecl_out = nodecl_make_err_expr(locus);
        nodecl_free(pointer_arg);
        nodecl_free(alignment);
        return;
    }
    else
    {
        int v = const_value_cast_to_signed_int(nodecl_get_constant(alignment));
        char is_power_of_two = (v && !(v & (v - 1))); // Bithack
        if (!is_power_of_two)
        {
            error_printf("%s: error: second argument of __assume_aligned argument must be a power of two constant\n",
                    nodecl_locus_to_str(alignment));
            *nodecl_out = nodecl_make_err_expr(locus);
            nodecl_free(pointer_arg);
            nodecl_free(alignment);
            return;
        }
    }

    *nodecl_out = nodecl_make_intel_assume_aligned(pointer_arg, alignment, get_void_type(), locus);
}
