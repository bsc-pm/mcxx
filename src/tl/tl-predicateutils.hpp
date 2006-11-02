#ifndef TL_PREDICATEUTILS_HPP
#define TL_PREDICATEUTILS_HPP

#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-predicate.hpp"

namespace TL
{
	template<const char* _ATTR>
	class PredicateBool : public Predicate
	{
		public:
			virtual bool operator()(const AST_t& ast) const
			{
                TL::Bool attr = ast.get_attribute(_ATTR);
                return attr;
			}
			virtual ~PredicateBool() { }
	};
}

#endif
