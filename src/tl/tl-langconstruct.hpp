#ifndef TL_LANGCONSTRUCT_HPP
#define TL_LANGCONSTRUCT_HPP

#include "tl-ast.hpp"

namespace TL
{
	class LangConstruct
	{
		protected:
			AST_t _ref;

			LangConstruct(const LangConstruct& ref)
			{
			}
		public:
			LangConstruct(AST_t ref)
				: _ref(ref)
			{
			}
	};
}

#endif // TL_LANGCONSTRUCT_HPP
