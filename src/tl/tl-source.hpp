#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include <string>
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "tl-scopelink.hpp"
#include "cxx-lexer.h"
#include "cxx-driver.h"
#include "cxx-scope.h"
#include "cxx-buildscope.h"

namespace TL
{
	class Source : public Object
	{
		private:
			std::string _code;

			virtual tl_type_t* get_extended_attribute(const std::string& str) const
			{
				return NULL;
			}
		public :
			Source()
				: _code("")
			{
			}

			virtual bool is_source() const
			{
				return true;
			}

			Source& operator<<(const std::string& str);
			AST_t* parse_global(TL::Scope* ctx, TL::ScopeLink* scope_link);
	};
}

#endif // TL_SOURCE_T_HPP
