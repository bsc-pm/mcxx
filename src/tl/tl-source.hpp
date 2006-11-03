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
			bool all_blanks() const;
		public :
			Source()
				: _code("")
			{
			}

            Source (const std::string& str)
                : _code(str)
            {
            }

			Source(const Source& src)
				: _code(src._code)
			{
			}

			Source& append_with_separator(Source src, const std::string& separator);

			virtual bool is_source() const
			{
				return true;
			}

            std::string get_source();

			Source& operator<<(const Source& src);
			Source& operator<<(const std::string& str);
			Source& operator<<(int n);

			AST_t parse_global(TL::Scope ctx, TL::ScopeLink scope_link);
			AST_t parse_statement(TL::Scope ctx, TL::ScopeLink scope_link);

			AST_t parse_expression(TL::Scope ctx);

			bool operator==(Source src) const;
			bool operator<(Source src) const;
			Source& operator=(Source src);
	};
}

#endif // TL_SOURCE_T_HPP
