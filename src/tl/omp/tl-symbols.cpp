#include "tl-omptransform.hpp"

namespace TL
{
    bool OpenMPTransform::is_nonstatic_member_function(FunctionDefinition function_definition)
    {
        IdExpression function_name = function_definition.get_function_name();
        Symbol function_symbol = function_name.get_symbol();

        // It must be a member
        if (!function_symbol.is_member())
        {
            return false;
        }

        Statement function_body = function_definition.get_function_body();
        Scope function_body_scope = function_body.get_scope();

        Symbol sym = function_body_scope.get_symbol_from_name("this");

        if (!sym.is_valid())
        {
            return false;
        }

        return true;
    }

    bool OpenMPTransform::is_unqualified_member_symbol(IdExpression id_expression, FunctionDefinition function_definition)
    {
        Symbol current_symbol = id_expression.get_symbol();
        Symbol function_symbol = function_definition.get_function_name().get_symbol();

        if (function_symbol.is_member()
                && current_symbol.is_member()
                && (function_symbol.get_class_type() 
                    == current_symbol.get_class_type()))
        {
            return 1;
        }

        return 0;
    }

    bool OpenMPTransform::is_function_accessible(IdExpression id_expression, 
            FunctionDefinition function_definition)
    {
        Symbol current_symbol = id_expression.get_symbol();

        Scope function_scope = function_definition.get_scope();
        Symbol function_visible_symbol = function_scope.get_symbol_from_id_expr(id_expression.get_ast());

        return (function_visible_symbol.is_valid()
                && function_visible_symbol == current_symbol);
    }
}
