#ifndef TL_PREDICATE_HPP
#define TL_PREDICATE_HPP

namespace TL
{
	class AST_t;
	class Predicate 
	{
		public :
			virtual bool operator()(const AST_t& ast) const = 0;
			virtual ~Predicate() { }
	};
}

#endif
