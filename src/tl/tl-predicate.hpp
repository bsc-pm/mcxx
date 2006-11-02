#ifndef TL_PREDICATE_HPP
#define TL_PREDICATE_HPP

#include "tl-ast.hpp"

namespace TL
{
	class AST_t;
	class Predicate 
	{
		public :
			virtual bool operator()(const AST_t& ast) const { return false; }
			virtual ~Predicate() { }
	};

}

#endif
