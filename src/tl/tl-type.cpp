#include "tl-type.hpp"

namespace TL
{
	std::string Type::get_declaration_str(const std::string& symbol_name) const
	{
		std::string before_declarator;
		std::string after_declarator;

		std::string result = get_type_name_str(_type_info, before_declarator, after_declarator);
		return result;
	}

	static void get_type_name_str(type_t* type_info, 
			std::string& before_declarator,
			std::string& after_declarator)
	{
	}

	static void std::string get_simple_type_name_str(simple_type_t* simple_type)
	{
	}
}
