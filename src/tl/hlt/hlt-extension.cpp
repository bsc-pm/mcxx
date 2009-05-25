#include "hlt-extension.hpp"

using namespace TL::HLT;

FunctionExtension::FunctionExtension(FunctionDefinition funct_def, Expression extension_amount)
    : _funct_def(funct_def), _extension_amount(extension_amount), _function_symbol(_funct_def.get_function_symbol())
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

    function_name << "_" << _function_symbol.get_name() << "_ext"
        ;

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
        is_const_extension = _extension_amount.is_constant();
        if (!is_const_extension)
        {
            extended_args << _extension_amount.get_type().get_declaration(_function_symbol.get_scope(), "_N");
        }
        else
        {
            bool valid = false;
            const_value = _extension_amount.evaluate_constant_int_expression(valid);
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
                << "{ int _N; " << statement_placeholder(placeholder) << "}";

            fake_context.parse_statement(function_body.get_ast(), function_body.get_scope_link());

            fake_dim_expr = new Expression(Source("_N").parse_expression(placeholder, function_body.get_scope_link()),
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
            upper_ext << "_N";
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
