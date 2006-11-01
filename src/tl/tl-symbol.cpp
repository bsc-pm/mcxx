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

	bool Symbol::operator<(Symbol s)
	{
		return this->_symbol < s._symbol;
	}

	Symbol& Symbol::operator=(Symbol s)
	{
		this->_symbol = s._symbol;
		return (*this);
	}

	bool Symbol::operator==(Symbol s)
	{
		return (this->_symbol == s._symbol);
	}
}
