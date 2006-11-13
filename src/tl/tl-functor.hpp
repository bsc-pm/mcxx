#ifndef TL_FUNCTOR_HPP
#define TL_FUNCTOR_HPP

#include <iostream>

namespace TL
{
	template <class Ret, class T>
	class Functor
	{
		public:
			virtual Ret operator()(T& t) const = 0; 

			virtual ~Functor() { }
	};

	template <class Ret, class T>
	class FunctionAdapter : Functor<Ret, T>
	{
		private:
			Ret (*_pf)(T&);
		public:
			FunctionAdapter(Ret (*pf)(T&))
				: _pf(pf)
			{
			}

			virtual Ret operator()(T& t) const 
			{
				return (_pf)(t);
			}

			virtual ~FunctionAdapter()
			{
			}
	};

	template <class Ret, class T, class Q>
	class MemberFunctionAdapter : public Functor<Ret, T>
	{
		private:
			Q& _q;
			Ret (Q::*_pmf)(T& t);
		public:
			MemberFunctionAdapter(Ret (Q::*pmf)(T& t), Q& q)
				: _q(q), _pmf(pmf)
			{
			}

			virtual Ret operator()(T& t) const
			{
				return (_q.*_pmf)(t);
			}

			virtual ~MemberFunctionAdapter()
			{
			}
	};

	template <class Ret, class T>
	FunctionAdapter<Ret, T> functor(Ret (*pf)(T&))
	{
		FunctionAdapter<Ret, T> result(pf);
		return result;
	}

	template <class Ret, class T, class Q>
	MemberFunctionAdapter<Ret, T, Q> functor(Ret (Q::*pmf)(T& t), Q& q)
	{
		MemberFunctionAdapter<Ret, T, Q> result(pmf, q);
		return result;
	}
}

#endif // TL_FUNCTOR_HPP
