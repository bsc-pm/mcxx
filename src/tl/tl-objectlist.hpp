#ifndef TL_OBJECTLIST_HPP
#define TL_OBJECTLIST_HPP

#include <vector>
#include "tl-functor.hpp"
#include "tl-predicate.hpp"

namespace TL
{

template <class T>
class ObjectList : public std::vector<T>
{
	private:
	public:
		ObjectList<T> filter(Predicate<T>& p)
		{
			ObjectList<T> result;
			for (typename ObjectList<T>::iterator it = this->begin();
					it != this->end();
					it++)
			{
				if (p(*it))
				{
					result.push_back(*it);
				}
			}

			return result;
		}

		template <class S>
		ObjectList<S> map(Functor<S, T>& f)
		{
			ObjectList<T> result;
			for (typename ObjectList<T>::iterator it = this->begin();
					it != this->end();
					it++)
			{
				S s(f(*it));
				result.push_back(s);
			}

			return result;
		}

		template <class S>
		ObjectList<S> map_filter(Predicate<T>& p, Functor<S, T>& f)
		{
			return (this->filter(p)).map(f);
		}
};

}

#endif // TL_OBJECTLIST_HPP
