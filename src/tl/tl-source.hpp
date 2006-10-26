#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include <string>
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "tl-scope.hpp"
#include "cxx-lexer.h"
#include "cxx-driver.h"

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

			Source& operator<<(const std::string& str)
			{
				_code += str;
				return *this;
			}

			AST_t* parse_global(TL::Scope* ctx)
			{
				const char* str = _code.c_str();

				mcxx_prepare_string_for_scanning(str);

				AST a;
				mcxxparse(&a);

				return new TL::AST_t(a);
			}
	};
}

#endif // TL_SOURCE_T_HPP
