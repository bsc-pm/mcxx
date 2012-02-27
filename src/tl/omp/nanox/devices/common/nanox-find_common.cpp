/*--------------------------------------------------------------------
  (C) Copyright 2006-2012 Barcelona Supercomputing Center
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

#include "nanox-find_common.hpp"
#include "tl-simd.hpp"

using namespace TL;
using namespace TL::Nanox;

bool FindFunction::do_(const AST_t& ast) const
{
    if (!ast.is_valid())
        return false;

    if (Expression::predicate(ast))
    {
        Expression expr(ast, _sl);
        if (expr.is_function_call())
        {
            expr = expr.get_called_expression();
            if (expr.is_id_expression())
            {
                IdExpression id_expr = expr.get_id_expression();
                if (id_expr.is_unqualified())
                {
                    if (id_expr.get_unqualified_part().compare(_func_name) == 0)
                        return true;
                }
            }
        }
    }
    return false;
};

bool FindAttribute::do_(const AST_t& ast) const
{
    if (GCCAttributeSpecifier::predicate(ast))
    {
        GCCAttributeSpecifier gcc_attr_spec(ast, _sl);
        ObjectList<GCCAttribute> gcc_attributes = gcc_attr_spec.get_gcc_attribute_list();

        if (gcc_attributes.contains(functor(&GCCAttribute::get_name), _attr_name))
        {
            return true;
        }
    }
    return false;
}
// {
//     ObjectList<AST_t> att_spec_ast_list =
//         ast.depth_subtrees(GCCAttributeSpecifier::predicate);
// 
//     for (ObjectList<AST_t>::iterator it = att_spec_ast_list.begin();
//             it != att_spec_ast_list.end();
//             it++)
//     {
//         ObjectList<GCCAttribute> att_list =
//             (GCCAttributeSpecifier(((AST_t)(*it)), _sl)).get_gcc_attribute_list();
// 
//         for (ObjectList<GCCAttribute>::iterator it = att_list.begin();
//                 it != att_list.end();
//                 it++)
//         {
//             if (((GCCAttribute)(*it)).get_name().compare(_attr_name) == 0)
//             {
//                 if(att_list.size() != 1)
//                     internal_error("'%s' GCCAttribute does not accept more GCCAttributes\n", _attr_name.c_str());
// 
//                 return true;
//             }
//         }
//     }
// }




