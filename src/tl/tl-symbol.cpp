#include "tl-symbol.hpp"
#include "tl-type.hpp"


namespace TL
{
	Type* Symbol::get_type() const
	{
		if (_symbol->type_information != NULL)
		{
			return new Type(_symbol->type_information);
		}
		return NULL;
	}

	std::string Symbol::get_name() const
	{
		return (_symbol->symbol_name != NULL) ? 
			std::string(_symbol->symbol_name) : 
			std::string("");
	}
}
