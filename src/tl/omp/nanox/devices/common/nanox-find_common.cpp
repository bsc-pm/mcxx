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




