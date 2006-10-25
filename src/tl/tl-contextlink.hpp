#ifndef TL_SCOPELINK_HPP
#define TL_SCOPELINK_HPP

#include "cxx-scopelink.h"
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-context.hpp"

namespace TL
{
	class ContextLink : public Object
	{
		private:
			scope_link_t* _scope_link;

			ContextLink(scope_link_t* scope_link)
				: _scope_link(scope_link)
			{
			}

		protected :
			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}
		public:
			Context* get_context(AST_t& ast);

			friend class CompilerPhaseRunner;
	};
}

#endif // TL_SCOPELINK_HPP
