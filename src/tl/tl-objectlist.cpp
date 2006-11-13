#include "tl-objectlist.hpp"
namespace TL
{

std::string concat_strings(const ObjectList<std::string>& string_list, const std::string& separator)
{
	std::string result;
	for (ObjectList<std::string>::const_iterator it = string_list.begin();
			it != string_list.end();
			it++)
	{
		if (it != string_list.begin())
		{
			result += separator;
		}
		result += *it;
	}

	return result;
}

}
