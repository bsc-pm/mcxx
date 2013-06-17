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




#include "hlt-extension.hpp"

using namespace TL::HLT;

FunctionExtension::FunctionExtension(FunctionDefinition funct_def)
    : _funct_def(funct_def), 
    _extension_amount(NULL), 
    _function_symbol(_funct_def.get_function_symbol()),
    _extended_function_name("")
{
}

TL::Source FunctionExtension::get_source()
{
    do_extension();
    return _extension_code;
}

void FunctionExtension::do_extension()
{
    Source return_type, function_name, extended_args, extended_body;

    _extension_code
        << return_type << " " << function_name << "(" << extended_args << ")"
        << extended_body
        ;

    Type function_type = _function_symbol.get_type();
    if (!function_type.returns().is_void())
    {
        _ostream << _funct_def.get_ast().get_locus() << ": warning: function should return void" << std::endl;
    }
    // This is so silly but maybe in the future we will work on an array
    return_type << "void";

    if (_extended_function_name != "")
    {
        function_name << _extended_function_name
            ;
    }
    else
    {
        function_name << "_" << _function_symbol.get_name() << "_ext"
            ;
    }

    bool has_ellipsis = false;
    ObjectList<Type> parameters = function_type.parameters(has_ellipsis);

    if (has_ellipsis)
    {
        _ostream << _funct_def.get_ast().get_locus() << ": warning: ellipsis is not valid when extending functions. Skipping" << std::endl;
        _extension_code = Source("");
    }
    else if (parameters.empty())
    {
        _ostream << _funct_def.get_ast().get_locus() << ": warning: function has zero parameters. Skipping" << std::endl;
        _extension_code = Source("");
    }
    else
    {
        bool is_const_extension = false;
        int const_value = 0;
        is_const_extension = _extension_amount != NULL && _extension_amount->is_constant();
        if (!is_const_extension)
        {
            extended_args << Type::get_int_type().get_declaration(_function_symbol.get_scope(), "N_");
        }
        else
        {
            bool valid = false;
            const_value = _extension_amount->evaluate_constant_int_expression(valid);
        }

        DeclaredEntity declared_entity = _funct_def.get_declared_entity();

        ObjectList<ParameterDeclaration> param_decl = declared_entity.get_parameter_declarations(has_ellipsis);

        Statement function_body = _funct_def.get_function_body();
        Expression *fake_dim_expr = NULL;
        Scope fake_dim_expr_sc;

        if (!is_const_extension)
        {
            // I hate having to do this
            AST_t placeholder;
            Source fake_context;
            fake_context
                << "{ int N_; " << statement_placeholder(placeholder) << "}";

            fake_context.parse_statement(function_body.get_ast(), function_body.get_scope_link());

            fake_dim_expr = new Expression(Source("N_").parse_expression(placeholder, function_body.get_scope_link()),
                    function_body.get_scope_link());

            fake_dim_expr_sc = function_body.get_scope_link().get_scope(placeholder);
        }
        else
        {
            Source src;
            src << const_value;
            fake_dim_expr = new Expression(
                    src.parse_expression(function_body.get_ast(), 
                        function_body.get_scope_link()), 
                    function_body.get_scope_link());
            fake_dim_expr_sc = function_body.get_scope_link().get_scope(function_body.get_ast());
        }

        ReplaceSrcIdExpression replacements(_funct_def.get_scope_link());
        for (ObjectList<ParameterDeclaration>::iterator it = param_decl.begin();
                it != param_decl.end();
                it++)
        {
            IdExpression id_expr = it->get_name();
            Symbol sym = id_expr.get_symbol();
            Type param_type = it->get_type();

            Type array_type(NULL);
            array_type = param_type.get_array_to(fake_dim_expr->get_ast(), fake_dim_expr_sc);

            // This expression is not valid

            extended_args.append_with_separator(
                    array_type.get_declaration(_function_symbol.get_scope(), id_expr),
                    ",");

            replacements.add_replacement(sym, "(" + sym.get_name() + "[_i])");
        }

        Source upper_ext;
        if (is_const_extension)
        {
            upper_ext << const_value;
        }
        else
        {
            upper_ext << "N_";
        }

        Source replaced_body;
        extended_body
            << "{"
            <<   "int _i;"
            <<   "for (_i = 0; _i < " << upper_ext << "; _i++)"
            <<   "{"
            <<   replacements.replace(function_body)
            <<   "}"
            << "}"
            ;

        delete fake_dim_expr;
    }
}

FunctionExtension& FunctionExtension::set_extended_function_name(const std::string name)
{
    this->_extended_function_name = name;
    return *this;
}

FunctionExtension& FunctionExtension::set_extension_amount(Expression expr)
{
    _extension_amount = new Expression(expr);
}
