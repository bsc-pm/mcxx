#ifndef TL_PREDICATEUTILS_HPP
#define TL_PREDICATEUTILS_HPP

#include "tl-builtin.hpp"
#include "tl-ast.hpp"
#include "tl-predicate.hpp"
#include "tl-objectlist.hpp"

namespace TL
{
	template<const char* _ATTR>
	class PredicateBool : public Predicate<AST_t>
	{
		public:
			virtual bool operator()(AST_t& ast) const
			{
                TL::Bool attr = ast.get_attribute(_ATTR);
                return attr;
			}
			virtual ~PredicateBool() { }
	};

	class PredicateAttr : public Predicate<AST_t>
	{
		private:
			const char* _attr_name;
		public:
			PredicateAttr(const char* attr_name)
				: _attr_name(attr_name)
			{
			}

			virtual bool operator()(AST_t& ast) const
			{
				return TL::Bool(ast.get_attribute(_attr_name));
			}
	};

	template <class T>
	class FunctionPredicate : public FunctionAdapter<bool, T>
	{
		public:
			FunctionPredicate(bool (*pf)(T&))
				: FunctionAdapter<bool, T>(pf)
			{
			}

			~FunctionPredicate()
			{
			}
	};

	template <class T, class Q>
	class MemberFunctionPredicate : public MemberFunctionAdapter<bool, T, Q>
	{
		public:
			MemberFunctionPredicate(bool (Q::*pmf)(T& t), Q& q)
				: MemberFunctionAdapter<bool, T, Q>(pmf, q)
			{
			}

			~MemberFunctionPredicate()
			{
			}
	};

	template <class T>
	FunctionPredicate<T> predicate(bool (*pf)(T&))
	{
		return FunctionPredicate<T>(pf);
	}

	template <class T, class Q>
	MemberFunctionPredicate<T, Q> predicate(bool (Q::* pf)(T& t), Q& q)
	{
		return MemberFunctionPredicate<T, Q>(pf, q);
	}

	template <class T>
	class InSetPredicate : public Predicate<T>
	{
		private:
			ObjectList<T>& _list;
		public:
			InSetPredicate(ObjectList<T>& list)
				: _list(list)
			{
			}

			virtual bool operator()(T& t) const
			{
				return (find(_list.begin(), _list.end(), t) != _list.end());
			}
	};

	template <class T>
	class NotInSetPredicate : public InSetPredicate<T>
	{
		public:
			NotInSetPredicate(ObjectList<T>& list)
				: InSetPredicate<T>(list)
			{
			}

			virtual bool operator()(T& t) const
			{
				return !(InSetPredicate<T>::operator()(t));
			}
	};

	template <class T>
	InSetPredicate<T> in_set(ObjectList<T>& list)
	{
		return InSetPredicate<T>(list);
	}

	template <class T>
	NotInSetPredicate<T> not_in_set(ObjectList<T>& list)
	{
		return NotInSetPredicate<T>(list);
	}
}

#endif
