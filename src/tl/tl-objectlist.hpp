#ifndef TL_OBJECTLIST_HPP
#define TL_OBJECTLIST_HPP

#include <vector>
#include <utility>
#include "tl-functor.hpp"
#include "tl-predicate.hpp"

namespace TL
{

template <class T>
class ObjectList : public std::vector<T>
{
	private:
		template <class Q>
		void reduction_helper(Q &result, typename ObjectList<T>::iterator it,
				const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter)
		{
			if (it == this->end())
			{
				result = neuter;
			}
			else
			{
				T& t = *it;
				reduction_helper(result, it + 1, red_func, neuter);

				std::pair<T, Q> arg(t, result);
				result = red_func(arg);
			}
		}

	public:
		ObjectList<T> filter(const Predicate<T>& p)
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
		ObjectList<S> map(const Functor<S, T>& f)
		{
			ObjectList<S> result;
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
		ObjectList<S> map_filter(const Predicate<T>& p, const Functor<S, T>& f)
		{
			return (this->filter(p)).map(f);
		}

		template <class Q>
		Q reduction(const Functor<Q, std::pair<T, Q> >& red_func, const Q& neuter)
		{
			Q result;
			reduction_helper(result, this->begin(), red_func, neuter);

			return result;
		}

		void append(const T& t)
		{
			this->push_back(t);
		}

		void append(const ObjectList<T>& t)
		{
			for (typename ObjectList<T>::const_iterator it = t.begin();
					it != t.end();
					it++)
			{
				this->append(*it);
			}
		}

		void insert(const T& t)
		{
			if (!contains(t))
			{
				this->push_back(t);
			}
		}

		void insert(const ObjectList<T>& t)
		{
			for (typename ObjectList<T>::const_iterator it = t.begin();
					it != t.end();
					it++)
			{
				this->insert(*it);
			}
		}
		
		bool contains(const T& t)
		{
			return (find(this->begin(), this->end(), t) != this->end());
		}

		template <class S>
		bool contains(const Functor<S, T>& f, const S& s)
		{
			for (typename ObjectList<T>::iterator it = this->begin();
					it != this->end();
					it++)
			{
				if (f(*it) == s)
				{
					return true;
				}
			}
			return false;
		}

		ObjectList<T> find(const T& t)
		{
			ObjectList<T> result;

			for (typename ObjectList<T>::iterator it = find(this->begin(), this->end(), t);
					it != this->end();
					it++)
			{
				result.append(*it);
			}
		}

		template <class S>
		ObjectList<T> find(const Functor<S, T>& f, const S& s)
		{
			ObjectList<S> result;

			for (typename ObjectList<T>::const_iterator it = this->begin();
					it != this->end();
					it++)
			{
				if (f(*it) == s)
				{
					result.append(*it);
				}
			}

			return result;
		}
};

std::string concat_strings(const ObjectList<std::string>& string_list, const std::string& separator = std::string(""));

}

#endif // TL_OBJECTLIST_HPP
