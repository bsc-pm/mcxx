#include "tl-type.hpp"
#include "tl-ast.hpp"
#include "cxx-utils.h"
#include "cxx-typeutils.h"
#include "cxx-scope.h"

namespace TL
{
	std::string Type::get_declaration_with_initializer(Scope sc, const std::string& symbol_name,
			const std::string& initializer) const
	{
        return get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), 
				initializer.c_str(), 0, NULL, NULL);
	}

	std::string Type::get_declaration_with_parameters(Scope sc,
			const std::string& symbol_name, ObjectList<std::string>& parameters)
	{
		char** parameter_names = NULL;
		int num_parameters = 0;
        char* result = get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), 
				"", 0, &num_parameters, &parameter_names);

		for (int i = 0; i < num_parameters; i++)
		{
			parameters.push_back(std::string(parameter_names[i]));
		}

		return result;
	}

    std::string Type::get_simple_declaration(Scope sc, const std::string& symbol_name) const
    {
        return get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), "", 0, NULL, NULL);
    }

    std::string Type::get_declaration(Scope sc, const std::string& symbol_name) const
    {
        return get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), "", 0, NULL, NULL);
    }

	Type Type::duplicate()
	{
		type_t* new_type_info = copy_type(_type_info);
		return Type(new_type_info);
	}

	Type Type::get_pointer_to()
	{
		Type result = this->duplicate();
		type_t* result_type = result._type_info;

		type_t* pointer_to = (type_t*)calloc(1, sizeof(*pointer_to));

		pointer_to->kind = TK_POINTER;
		pointer_to->pointer = (pointer_info_t*)calloc(1, sizeof(*(pointer_to->pointer)));
		pointer_to->pointer->pointee = result_type;

		result._type_info = pointer_to;

		return result;
	}

	Type Type::get_array_to(AST_t array_expr, Scope scope)
	{
		Type result = this->duplicate();
		type_t* result_type = result._type_info;

		type_t* array_to = (type_t*)calloc(1, sizeof(*array_to));

		array_to->kind = TK_ARRAY;
		array_to->array = (array_info_t*)calloc(1, sizeof(*(array_to->array)));
		array_to->array->element_type = result_type;
		array_to->array->array_expr = array_expr._ast;
		array_to->array->array_expr_scope = scope._st;

		result._type_info = array_to;

		return result;
	}

	bool Type::operator==(Type t) const
	{
		return this->_type_info == t._type_info;
	}

	bool Type::operator!=(Type t) const
	{
		return !(this->operator==(t));
	}

	bool Type::operator<(Type t) const
	{
		return this->_type_info < t._type_info;
	}

	Type& Type::operator=(Type t)
	{
		this->_type_info = t._type_info;
		return (*this);
	}

	bool Type::is_builtin_type() const
	{
		return (is_fundamental_type(_type_info));
	}

	Type::BuiltinType Type::builtin_type(TypeModifier& type_modif) const
	{
		if (is_builtin_type())
		{
			type_t* type_info = _type_info;
			type_info = advance_over_typedefs(type_info);

			if ((type_info->cv_qualifier & CV_CONST) == CV_CONST)
			{
				type_modif |= TypeModifier::CONST;
			}

			if ((type_info->cv_qualifier & CV_VOLATILE) == CV_VOLATILE)
			{
				type_modif |= TypeModifier::VOLATILE;
			}

			if ((type_info->cv_qualifier & CV_RESTRICT) == CV_RESTRICT)
			{
				type_modif |= TypeModifier::RESTRICT;
			}

			if (type_info->type->is_short)
			{
				type_modif |= TypeModifier::SHORT;
			}
			else if (type_info->type->is_long)
			{
				if (type_info->type->is_long == 1)
				{
					type_modif |= TypeModifier::LONG;
				}
				else
				{
					type_modif |= TypeModifier::LONG_LONG;
				}
			}

			if (type_info->type->is_unsigned)
			{
				type_modif |= TypeModifier::UNSIGNED;
			}

			switch ((int)(type_info->type->builtin_type))
			{
				case BT_INT:
					return BuiltinType::INT;
				case BT_BOOL:
					return BuiltinType::BOOL;
				case BT_FLOAT:
					return BuiltinType::FLOAT;
				case BT_DOUBLE:
					return BuiltinType::DOUBLE;
				case BT_CHAR:
					return BuiltinType::CHAR;
				case BT_WCHAR:
					return BuiltinType::WCHAR;
				case BT_VOID:
					return BuiltinType::VOID;
				default:
					return BuiltinType::UNKNOWN;
			}
		}

		return BuiltinType::UNKNOWN;
	}

	Type::BuiltinType Type::builtin_type() const
	{
		TypeModifier type_modif;
		return builtin_type(type_modif);
	}

	bool Type::is_pointer() const
	{
		return (is_pointer_type(_type_info));
	}

	bool Type::is_array() const
	{
		return (is_array_type(_type_info));
	}

	bool Type::is_reference() const
	{
		return (is_reference_type(_type_info));
	}

	bool Type::is_function() const
	{
		return is_function_type(_type_info);
	}

	bool Type::is_dependent() const
	{
		return (is_dependent_type(_type_info, default_decl_context));
	}

	Type Type::returns() const
	{
		return function_return_type(_type_info);
	}

	Type Type::points_to() const
	{
		return pointer_pointee_type(_type_info);
	}

	Type Type::array_element() const
	{
		return array_element_type(_type_info);
	}

	Type Type::references_to() const
	{
		return reference_referenced_type(_type_info);
	}

	bool Type::is_direct_type() const
	{
		return ::is_direct_type(_type_info);
	}
}
