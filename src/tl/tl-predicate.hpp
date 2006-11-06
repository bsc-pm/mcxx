#ifndef TL_PREDICATE_HPP
#define TL_PREDICATE_HPP

#include "tl-functor.hpp"

namespace TL
{
	template <class T>
	class Predicate : public Functor<bool, T>
	{
		public:
			~Predicate() { }
	};

}

#endif
