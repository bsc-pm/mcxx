#include "tl-type.hpp"
#include "tl-ast.hpp"
#include "cxx-utils.h"
#include "cxx-scope.h"

namespace TL
{
    std::string Type::get_simple_declaration_str(const std::string& symbol_name) const
    {
        return get_declaration_str_internal(_type_info, symbol_name, true);
    }

    std::string Type::get_parameter_declaration_str(const std::string& symbol_name) const
    {
        return get_declaration_str_internal(_type_info, symbol_name, false);
    }

    std::string Type::get_declaration_str_internal(type_t* type_info, 
            const std::string& symbol_name, bool semicolon)
    {
        std::string base_type_name = get_simple_type_name_str(type_info);
        std::string declarator_name = get_type_name_str(type_info, symbol_name);

        std::string result;

        result = base_type_name 
            + std::string(" ")
            + declarator_name
            + std::string(semicolon ? ";" : "");

        return result;
    }

    std::string Type::get_cv_qualifier_str(type_t* type_info)
    {
        std::string result = std::string("");

        if (BITMAP_TEST(type_info, CV_CONST))
        {
            result += "const ";
        }

        if (BITMAP_TEST(type_info, CV_VOLATILE))
        {
            result += "volatile ";
        }

        if (BITMAP_TEST(type_info, CV_RESTRICT))
        {
            result += "restricted ";
        }

        return result;
    }

    std::string Type::get_type_name_str(type_t* type_info, 
            const std::string &symbol_name)
    {
        std::string left(""), right("");
        get_type_name_str_internal(type_info, symbol_name, left, right);

        return left + right;
    }

    bool Type::declarator_needs_parentheses(type_t* type_info)
    {
        bool result = false;
        if (type_info->kind == TK_POINTER_TO_MEMBER
                || type_info->kind == TK_POINTER
                || type_info->kind == TK_REFERENCE)
        {
            type_t* pointee = type_info->pointer->pointee;
            result = (pointee->kind != TK_POINTER_TO_MEMBER
                    && pointee->kind != TK_POINTER
                    && pointee->kind != TK_REFERENCE
                    && pointee->kind != TK_DIRECT);
        }

        return result;
    }

    void Type::get_type_name_str_internal(type_t* type_info, 
            const std::string &symbol_name,
            std::string& left,
            std::string& right)
    {
        std::string result;
        switch ((int)(type_info->kind))
        {
            case TK_DIRECT :
                {
                    left = left + symbol_name;
                    break;
                }
            case TK_POINTER :
                {
                    if (declarator_needs_parentheses(type_info))
                    {
                        left = left + "(";
                    }

                    left = left + "*";

                    get_type_name_str_internal(type_info->pointer->pointee, symbol_name, left, right);

                    if (declarator_needs_parentheses(type_info))
                    {
                        right = ")" + right;
                    }
                    break;
                }
            case TK_POINTER_TO_MEMBER :
                {
                    break;
                }
            case TK_REFERENCE :
                {
                    if (declarator_needs_parentheses(type_info))
                    {
                        left = left + "(";
                    }

                    left = left + "&";

                    get_type_name_str_internal(type_info->pointer->pointee, symbol_name, left, right);

                    if (declarator_needs_parentheses(type_info))
                    {
                        right = ")" + right;
                    }
                    break;
                }
            case TK_ARRAY :
                {
                    get_type_name_str_internal(type_info->array->element_type, symbol_name, left, right);

                    right = std::string("[")
                        + TL::AST_t(type_info->array->array_expr).prettyprint()
                        + std::string("]") + right;
                    break;
                }
            case TK_FUNCTION :
                {
                    get_type_name_str_internal(type_info->function->return_type, symbol_name, left, right);

                    std::string prototype;
                    prototype = std::string("(");
                    for (int i = 0; i < type_info->function->num_parameters; i++)
                    {
                        if (i > 0)
                        {
                            prototype += std::string(", ");
                        }

                        if (type_info->function->parameter_list[i]->is_ellipsis)
                        {
                            prototype += std::string("...");
                        }
                        else
                        {
                            // Abstract declarator
                            prototype += get_declaration_str_internal(type_info->function->parameter_list[i]->type_info, "", false);
                        }
                    }
                    prototype += std::string(")");

                    right = right + prototype;
                    break;
                }
            default:
                {
                    std::cerr << "Unknown type kind '" << (int)type_info->type << "'" << std::endl;
                    break;
                }
        }
    }

    std::string Type::get_simple_type_name_str(type_t* type_info)
    {
        std::string result;
        switch ((int)(type_info->kind))
        {
            case TK_DIRECT :
                {
                    TL::AST_t type_name(type_info->type->typeof_expr);
                    result = type_name.prettyprint();
                    break;
                }
            case TK_FUNCTION :
                {
                    result = get_simple_type_name_str(type_info->function->return_type);
                    break;
                }
            case TK_POINTER :
            case TK_REFERENCE :
            case TK_POINTER_TO_MEMBER :
                {
                    result = get_simple_type_name_str(type_info->pointer->pointee);
                    break;
                }
            case TK_ARRAY :
                {
                    result = get_simple_type_name_str(type_info->array->element_type);
                    break;
                }
            default:
                break;
        }
        return result;
    }
}
