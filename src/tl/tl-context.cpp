#include "tl-context.hpp"

namespace TL
{
	bool Context::in_function()
	{
		return _in_function;
	}

	bool Context::function_is_member()
	{
		return _function_is_member;
	}

	bool Context::function_is_template()
	{
		return _function_is_template;
	}

	std::string Context::unqualified_function_name()
	{
		return std::string();
	}

	std::string Context::qualification_function_name()
	{
		return std::string();
	}

	std::string Context::function_name()
	{
		return std::string();
	}

	std::string Context::file_name()
	{
		return std::string();
	}

	int Context::line()
	{
		return _line;
	}
}
