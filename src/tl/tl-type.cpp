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
				initializer.c_str(), 0);
	}

    std::string Type::get_simple_declaration(Scope sc, const std::string& symbol_name) const
    {
        return get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), "", 0);
    }

    std::string Type::get_declaration(Scope sc, const std::string& symbol_name) const
    {
        return get_declaration_string_internal(_type_info, sc._st, symbol_name.c_str(), "", 0);
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
}
