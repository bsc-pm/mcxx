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




#include "hlt-inline.hpp"
#include "cxx-utils.h"
#include "uniquestr.h"

using namespace TL;
using namespace TL::HLT;

FunctionCallInline::FunctionCallInline(TL::Expression function_call)
    : _function_call(function_call), _function_symbol(NULL)
{
}

const char* FunctionCallInline::inline_prettyprint_callback(AST _a, void* data)
{
    AST_t a(_a);
    FunctionCallInline* _this = reinterpret_cast<FunctionCallInline*>(data);

    if (ReturnStatement::predicate(a))
    {
        ReturnStatement ret_stmt(a, _this->_function_call.get_scope_link());
        Expression expr = ret_stmt.get_return_expression();

        Source src;
        src 
            << "_return = (" << expr << ");"
            << "goto _end;"
            ;

        return uniquestr(src.get_source().c_str());
    }
    else if (IdExpression::predicate(a))
    {
        IdExpression id_expr(a, _this->_function_call.get_scope_link());

        Symbol sym = id_expr.get_symbol();

        if (sym.is_parameter())
        {
            Source src;
            src << "_p_" << sym.get_parameter_position();

            return uniquestr(src.get_source().c_str());
        }
        else if (_this->_function_symbol.is_member()
                && id_expr.is_unqualified()
                && sym.is_member()
                && sym.get_class_type().is_same_type(_this->_function_symbol.get_class_type()))
        {
            Source src;
            src << "_this." << id_expr;

            return uniquestr(src.get_source().c_str());
        }
        else
            return NULL;
    }
    else
        return NULL;
}

Source FunctionCallInline::get_source()
{
    // Do nothing if it is not a function call
    if (!_function_call.is_function_call())
        return _function_call.prettyprint();

    // At the moment a.b() is not supported
    if (!_function_call.is_named_function_call())
        return _function_call.prettyprint();

    _function_symbol = _function_call.get_called_entity();

    AST_t definition_tree = _function_symbol.get_definition_tree();

    if (!definition_tree.is_valid())
        return _function_call.prettyprint();

    ObjectList<Expression> arguments = _function_call.get_argument_list();

    Source result;

    Source parameter_declarations, 
           inlined_function_body, 
           return_code;

    // Declare parameters
    Type function_type = _function_symbol.get_type();
    bool has_ellipsis = false;
    ObjectList<Type> parameters = function_type.parameters(has_ellipsis);

    int i = 0;
    for (ObjectList<Type>::iterator it = parameters.begin();
            it != parameters.end();
            it++, i++)
    {
        Source param_name;
        param_name
            << "_p_" << i;

        Source init;

        parameter_declarations
            << it->get_declaration(_function_call.get_scope(),
                    param_name)
            << init
            << ";"
            ;

        C_LANGUAGE()
        {
            init << " = ";
        }
        init << "(" << arguments[i] << ")" 
            ;
    }

    // Return type only if it is not void
    Type return_type = function_type.returns();

    if (return_type.is_valid()
            && !return_type.is_void())
    {
        // This is only valid in C, in C++ we will use an aligned char and a
        // new with placement
        parameter_declarations
            << return_type.get_declaration(_function_call.get_scope(), 
                    "_return") << ";";

        return_code
            << "_end: _return;"
            ;
    }

    FunctionDefinition funct_def(definition_tree, _function_call.get_scope_link());

    const char *c = prettyprint_in_buffer_callback(funct_def.get_function_body().get_ast().get_internal_ast(), 
            &FunctionCallInline::inline_prettyprint_callback, (void*)this);

    if (c != NULL)
    {
        inlined_function_body << std::string(c);
    }

    // The returned pointer came from C code, so 'free' it
    DELETE((void*)c);

    result
        << "({"
        << parameter_declarations
        << inlined_function_body
        << return_code
        << "})"
        ;

    return result;
}
