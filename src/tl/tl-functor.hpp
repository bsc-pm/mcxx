#ifndef TL_FUNCTOR_HPP
#define TL_FUNCTOR_HPP

#include <iostream>

namespace TL
{
	template <class Ret, class T>
	class Functor
	{
		public:
			virtual Ret operator()(T& t) = 0; 

			virtual ~Functor() { }
	};
}

#endif // TL_FUNCTOR_HPP
