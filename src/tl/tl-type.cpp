#include "tl-type.hpp"
#include "tl-ast.hpp"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"
#include <gc.h>

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
        std::string result = std::string(" ");

        if (BITMAP_TEST(type_info->cv_qualifier, CV_CONST))
        {
            result += "const ";
        }

        if (BITMAP_TEST(type_info->cv_qualifier, CV_VOLATILE))
        {
            result += "volatile ";
        }

        if (BITMAP_TEST(type_info->cv_qualifier, CV_RESTRICT))
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

					left = left + get_cv_qualifier_str(type_info);
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
                    if (declarator_needs_parentheses(type_info))
                    {
                        left = left + "(";
                    }

					left = left + std::string(type_info->pointer->pointee_class->symbol_name);

                    left = left + "::";
					left = left + "*";
					left = left + get_cv_qualifier_str(type_info);

                    get_type_name_str_internal(type_info->pointer->pointee, symbol_name, left, right);

                    if (declarator_needs_parentheses(type_info))
                    {
                        right = ")" + right;
                    }
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
                    prototype += std::string(") ");
					prototype += get_cv_qualifier_str(type_info);

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

	std::string Type::get_simple_type_name_str_internal(simple_type_t* simple_type)
	{
		std::string result;
		switch ((int)simple_type->kind)
		{
			case STK_USER_DEFINED :
			case STK_TYPEOF :
				{
					TL::AST_t type_name(simple_type->typeof_expr);
					result = type_name.prettyprint();
					break;
				}
			case STK_VA_LIST :
				{
					result = std::string("__builtin_va_list");
					break;
				}
			case STK_BUILTIN_TYPE :
				{
					if (simple_type->is_unsigned)
					{
						result = "unsigned ";
					}
					else if (simple_type->is_signed)
					{
						result = "signed ";
					}

					if (simple_type->is_long == 1)
					{
						result += "long ";
					}
					else if (simple_type->is_long >= 2)
					{
						result += "long long ";
					}
					else if (simple_type->is_short)
					{
						result += "short ";
					}

					switch ((int)simple_type->builtin_type)
					{
						case BT_INT :
							{
								result += "int ";
								break;
							}
						case BT_CHAR :
							{
								result += "char ";
								break;
							}
						case BT_WCHAR :
							{
								result += "wchar_t ";
								break;
							}
						case BT_FLOAT :
							{
								result += "float ";
								break;
							}
						case BT_DOUBLE :
							{
								result += "double ";
								break;
							}
						case BT_BOOL :
							{
								result += "bool ";
								break;
							}
						case BT_VOID :
							{
								result += "void ";
								break;
							}
						case BT_UNKNOWN :
							{
								result += " ";
								break;
							}
						default :
							break;
					}
					break;
				}
			default:
				break;
		}

		return result;
	}

    std::string Type::get_simple_type_name_str(type_t* type_info)
    {
        std::string result;
        switch ((int)(type_info->kind))
        {
            case TK_DIRECT :
                {
					return get_cv_qualifier_str(type_info) 
						+ get_simple_type_name_str_internal(type_info->type);
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

	Type* Type::duplicate()
	{
		type_t* new_type_info = copy_type(_type_info);
		return new Type(new_type_info);
	}

	Type* Type::get_pointer_to()
	{
		Type* result = this->duplicate();
		type_t* result_type = result->_type_info;

		type_t* pointer_to = (type_t*)GC_CALLOC(1, sizeof(*pointer_to));

		pointer_to->kind = TK_POINTER;
		pointer_to->pointer = (pointer_info_t*)GC_CALLOC(1, sizeof(*(pointer_to->pointer)));
		pointer_to->pointer->pointee = result_type;

		result->_type_info = pointer_to;

		return result;
	}

	Type* Type::get_array_to(AST_t *array_expr, Scope* scope)
	{
		Type* result = this->duplicate();
		type_t* result_type = result->_type_info;

		type_t* array_to = (type_t*)GC_CALLOC(1, sizeof(*array_to));

		array_to->kind = TK_ARRAY;
		array_to->array = (array_info_t*)GC_CALLOC(1, sizeof(*(array_to->array)));
		array_to->array->element_type = result_type;
		array_to->array->array_expr = array_expr->_ast;
		array_to->array->array_expr_scope = scope->_st;

		result->_type_info = array_to;

		return result;
	}
}
