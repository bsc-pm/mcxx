#ifndef TL_SOURCE_T_HPP
#define TL_SOURCE_T_HPP

#include <string>
#include "tl-object.hpp"
#include "tl-ast.hpp"
#include "cxx-lexer.h"
#include "cxx-driver.h"

namespace TL
{
	class Source : public Object
	{
		private:
			std::string _code;
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

			AST_t* parse()
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
