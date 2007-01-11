#include <string>
#include "cxx-driver.h"
#include "tl-externalvars.hpp"

namespace TL
{
	std::string ExternalVars::get(const std::string& name, const std::string& default_val)
	{
		for (int i = 0; i < compilation_options.num_external_vars; i++)
		{
			std::string variable = compilation_options.external_vars[i]->name;
			
			if (variable == name)
			{
				return compilation_options.external_vars[i]->value;
			}
		}

		return default_val;
	}
}
