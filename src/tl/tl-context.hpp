#ifndef TL_CONTEXT_HPP
#define TL_CONTEXT_HPP

#include "tl-scopelink.hpp"
#include <string>

namespace TL
{
	struct Context
	{
		public:
			ScopeLink scope_link;

			Context(ScopeLink _scope_link)
				: scope_link(_scope_link)
			{
			}
	};
}

#endif // TL_CONTEXT_HPP
