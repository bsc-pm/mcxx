#ifndef TL_SCOPELINK_HPP
#define TL_SCOPELINK_HPP

#include "cxx-scopelink.h"
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"

namespace TL
{
	class ScopeLink : public Object
	{
		private:
			scope_link_t* _scope_link;

			ScopeLink(scope_link_t* scope_link)
				: _scope_link(scope_link)
			{
			}
		protected :
			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}
		public:
			Scope get_scope(AST_t ast);

            ScopeLink(const Object& obj)
            {
                const ScopeLink* sl = dynamic_cast<const ScopeLink*>(&obj);
                this->_scope_link = sl->_scope_link;
            }

            ScopeLink(const ScopeLink& sl)
                : _scope_link(sl._scope_link)
            {
            }

			ScopeLink& operator=(ScopeLink sl);
			bool operator==(ScopeLink sl);

			friend class Source;
			friend class CompilerPhaseRunner;
	};
}

#endif // TL_SCOPELINK_HPP
