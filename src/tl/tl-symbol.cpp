#include "tl-symbol.hpp"
#include "tl-type.hpp"


namespace TL
{
	Type Symbol::get_type() const
	{
		Type result(_symbol->type_information);
        return result;
	}

	std::string Symbol::get_name() const
	{
		return (_symbol->symbol_name != NULL) ? 
			std::string(_symbol->symbol_name) : 
			std::string("");
	}

	bool Symbol::operator<(Symbol s) const
	{
		return this->_symbol < s._symbol;
	}

	Symbol& Symbol::operator=(Symbol s)
	{
		this->_symbol = s._symbol;
		return (*this);
	}

	bool Symbol::operator==(Symbol s) const
	{
		return (this->_symbol == s._symbol);
	}

	bool Symbol::operator!=(Symbol s) const
	{
		return !(this->operator==(s));
	}

	const Symbol Symbol::invalid()
	{
		return Symbol(NULL);
	}

	bool Symbol::is_invalid() const
	{
		return (*this == invalid());
	}

	bool Symbol::is_valid() const
	{
		return !is_invalid();
	}

	// This should be subclassed since it is C/C++ specific
	bool Symbol::is_variable() const
	{
		return (this->_symbol->kind == SK_VARIABLE);
	}

	bool Symbol::is_typename() const
	{
		return (this->_symbol->kind == SK_TYPEDEF
				|| this->_symbol->kind == SK_ENUM
				|| this->_symbol->kind == SK_CLASS
				|| this->_symbol->kind == SK_TEMPLATE_PRIMARY_CLASS
				|| this->_symbol->kind == SK_TEMPLATE_SPECIALIZED_CLASS
				|| this->_symbol->kind == SK_TEMPLATE_TYPE_PARAMETER);
	}
}
