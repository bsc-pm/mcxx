/*--------------------------------------------------------------------
  (C) Copyright 2006-2013 Barcelona Supercomputing Center
                          Centro Nacional de Supercomputacion
  
  This file is part of Mercurium C/C++ source-to-source compiler.
  
  See AUTHORS file in the top level directory for information
  regarding developers and contributors.
  
  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 3 of the License, or (at your option) any later version.
  
  Mercurium C/C++ source-to-source compiler is distributed in the hope
  that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the GNU Lesser General Public License for more
  details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with Mercurium C/C++ source-to-source compiler; if
  not, write to the Free Software Foundation, Inc., 675 Mass Ave,
  Cambridge, MA 02139, USA.
--------------------------------------------------------------------*/




#ifndef TL_PREDICATEUTILS_HPP
#define TL_PREDICATEUTILS_HPP

#include "tl-common.hpp"
#include "tl-builtin.hpp"
#include "tl-predicate.hpp"
#include "tl-objectlist.hpp"

namespace TL
{
    //! Predicate that always returns true
    template<class T>
    class AlwaysTrue : public Predicate<T>
    {
        private:
        public:
            AlwaysTrue() {}
            virtual bool do_(typename AlwaysTrue::ArgType t) const
            {
                return true;
            }
    };

    //! Predicate that always returns false
    template<class T>
    class AlwaysFalse : public Predicate<T>
    {
        private:
        public:
            AlwaysFalse() {}
            virtual bool do_(typename AlwaysFalse::ArgType t) const
            {
                return false;
            }
    };

    //! Builds a predicate after a function
    template <class T>
    class FunctionPredicateVal : public Predicate<T>
    {
        private:
            FunctionAdapterVal<bool, T> _funct_adapter;
        public:
            FunctionPredicateVal(bool (*pf)(T))
                : _funct_adapter(pf)
            {
            }

            virtual bool do_(typename FunctionPredicateVal::ArgType t) const
            {
                return _funct_adapter(t);
            }

            ~FunctionPredicateVal()
            {
            }
    };

    template <class T>
    class FunctionPredicateRef : public Predicate<T&>
    {
        private:
            FunctionAdapterRef<bool, T> _funct_adapter;
        public:
            FunctionPredicateRef(bool (*pf)(T&))
                : _funct_adapter(pf)
            {
            }

            virtual bool do_(typename FunctionPredicateRef::ArgType t) const
            {
                return _funct_adapter(t);
            }

            ~FunctionPredicateRef()
            {
            }
    };

    template <class T>
    class FunctionPredicateConstRef : public Predicate<T>
    {
        private:
            FunctionAdapterConstRef<bool, T> _funct_adapter;
        public:
            FunctionPredicateConstRef(bool (*pf)(const T&))
                : _funct_adapter(pf)
            {
            }

            virtual bool do_(typename FunctionPredicateConstRef::ArgType t) const
            {
                return _funct_adapter(t);
            }

            ~FunctionPredicateConstRef()
            {
            }
    };

    //! Builds a predicate after a member function
    template <class T, class Q>
    class MemberFunctionPredicateVal : public Predicate<T>
    {
        private:
            MemberFunctionAdapterVal<bool, T, Q> mem_funct_adapter;
        public:
            MemberFunctionPredicateVal(bool (Q::*pmf)(T t), Q& q)
                : mem_funct_adapter(pmf, q)
            {
            }

            virtual bool do_(typename MemberFunctionPredicateVal::ArgType t) const
            {
                return mem_funct_adapter(t);
            }

            ~MemberFunctionPredicateVal()
            {
            }
    };

    //! Builds a predicate after a member function
    template <class T, class Q>
    class MemberFunctionPredicateRef : public Predicate<T&>
    {
        private:
            MemberFunctionAdapterRef<bool, T, Q> mem_funct_adapter;
        public:
            MemberFunctionPredicateRef(bool (Q::*pmf)(T &t), Q& q)
                : mem_funct_adapter(pmf, q)
            {
            }

            virtual bool do_(typename MemberFunctionPredicateRef::ArgType t) const
            {
                return mem_funct_adapter(t);
            }

            ~MemberFunctionPredicateRef()
            {
            }
    };

    //! Builds a predicate after a member function
    template <class T, class Q>
    class MemberFunctionPredicateConstRef : public Predicate<T>
    {
        private:
            MemberFunctionAdapterConstRef<bool, T, Q> mem_funct_adapter;
        public:
            MemberFunctionPredicateConstRef(bool (Q::*pmf)(T &t), Q& q)
                : mem_funct_adapter(pmf, q)
            {
            }

            virtual bool do_(typename MemberFunctionPredicateConstRef::ArgType t) const
            {
                return mem_funct_adapter(t);
            }

            ~MemberFunctionPredicateConstRef()
            {
            }
    };

    //! Builds a predicate after a member function of this class
    template <class T>
    class ThisMemberFunctionPredicate : public Predicate<T>
    {
        private:
            ThisMemberFunctionAdapter<bool, T> this_mem_funct_adapter;
        public:
            ThisMemberFunctionPredicate(bool (T::*pmf)())
                : this_mem_funct_adapter(pmf)
            {
            }

            virtual bool do_(typename ThisMemberFunctionPredicate::ArgType t) const
            {
                return this_mem_funct_adapter(t);
            }

            ~ThisMemberFunctionPredicate()
            {
            }
    };

    //! Builds a predicate after a member function of this const class
    template <class T>
    class ThisMemberFunctionConstPredicate : public Predicate<T>
    {
        private:
                ThisMemberFunctionConstAdapter<bool, T> this_mem_funct_adapter;
        public:
            ThisMemberFunctionConstPredicate(bool (T::*pmf)() const)
                : this_mem_funct_adapter(pmf)
            {
            }

            virtual bool do_(typename ThisMemberFunctionConstPredicate::ArgType t) const
            {
                return this_mem_funct_adapter(t);
            }

            ~ThisMemberFunctionConstPredicate()
            {
            }
    };

    //! Adaptor function to create predicates after a non-member function returning bool
    template <class T>
    FunctionPredicateVal<T> predicate(bool (*pf)(T))
    {
        return FunctionPredicateVal<T>(pf);
    }

    template <class T>
    FunctionPredicateRef<T> predicate(bool (*pf)(T&))
    {
        return FunctionPredicateRef<T>(pf);
    }

    template <class T>
    FunctionPredicateConstRef<T> predicate(bool (*pf)(const T&))
    {
        return FunctionPredicateConstRef<T>(pf);
    }

    //! Adaptor function to create predicates after a member function of a given object returning bool
    template <class T, class Q>
    MemberFunctionPredicateVal<T, Q> predicate(bool (Q::* pf)(T t), Q& q)
    {
        return MemberFunctionPredicateVal<T, Q>(pf, q);
    }

    template <class T, class Q>
    MemberFunctionPredicateRef<T, Q> predicate(bool (Q::* pf)(T& t), Q& q)
    {
        return MemberFunctionPredicateRef<T, Q>(pf, q);
    }

    template <class T, class Q>
    MemberFunctionPredicateConstRef<T, Q> predicate(bool (Q::* pf)(const T& t), Q& q)
    {
        return MemberFunctionPredicateConstRef<T, Q>(pf, q);
    }

    //! Adaptor function to create predicates after a member function of a given object returning bool
    template <class T>
    ThisMemberFunctionPredicate<T> predicate(bool (T::* pf)())
    {
        return ThisMemberFunctionPredicate<T>(pf);
    }

    //! Adaptor function to create predicates after a member function of a given const object returning bool
    template <class T>
    ThisMemberFunctionConstPredicate<T> predicate(bool (T::* pf)() const)
    {
        return ThisMemberFunctionConstPredicate<T>(pf);
    }

    //! Class of predicates useful to check whether an element is in a list
    template <class T>
    class InSetPredicate : public Predicate<T>
    {
        private:
            ObjectList<T>& _list;
        public:
            //! Constructor
            /*!
             * \param list The set related to this predicate
             */
            InSetPredicate(ObjectList<T>& list)
                : _list(list)
            {
            }

            //! States whether the given element is in the set used to create the predicate
            virtual bool do_(typename InSetPredicate::ArgType t) const
            {
                return (find(_list.begin(), _list.end(), t) != _list.end());
            }
    };

    //! Class of predicates useful to check whether an element is in a list with a given comparator
    template <class T, class Q>
    class InSetPredicateFunctor : public Predicate<T>
    {
        private:
            ObjectList<Q> _list;
            const Functor<Q, T>& _f;
        public:
            //! Constructor
            /*!
             * \param list The set of elements of Q related to this predicate
             * \param f The functor type T returning type Q
             */
            InSetPredicateFunctor(ObjectList<Q>& list, const Functor<Q, T>& f)
                : _list(list), _f(f)
            {
            }

            //! States whether the given functor yields a value already in the list
            /*
             * \param t The value of type T
             *
             * Functor \a f will be applied to \a t and the resulting value used
             * for comparison.
             */
            virtual bool do_(typename InSetPredicateFunctor::ArgType t) const
            {
                return (find(_list.begin(), _list.end(), _f(t)) != _list.end());
            }
    };

    //! The opposite of InSetPredicate
    template <class T>
    class NotInSetPredicate : public InSetPredicate<T>
    {
        public:
            NotInSetPredicate(ObjectList<T>& list)
                : InSetPredicate<T>(list)
            {
            }

            virtual bool do_(typename NotInSetPredicate::ArgType t) const
            {
                return !(InSetPredicate<T>::do_(t));
            }
    };

    //! The opposite of InSetPredicateFunctor
    template <class T, class Q>
    class NotInSetPredicateFunctor : public InSetPredicateFunctor<T, Q>
    {
        public:
            NotInSetPredicateFunctor(ObjectList<Q>& list, const Functor<Q, T>& f)
                : InSetPredicateFunctor<T, Q>(list, f)
            {
            }

            virtual bool do_(typename NotInSetPredicateFunctor::ArgType t) const
            {
                return !(InSetPredicateFunctor<T, Q>::do_(t));
            }
    };

    //! Adaptor function to create predicates to check list membership
    template <class T>
    InSetPredicate<T> in_set(ObjectList<T>& list)
    {
        return InSetPredicate<T>(list);
    }

    //! Adaptor function to create predicates to check list membership with a given functor
    template <class T, class Q>
    InSetPredicateFunctor<T, Q> in_set(ObjectList<Q>& list, const Functor<Q, T>& f)
    {
        return InSetPredicateFunctor<T, Q>(list, f);
    }

    //! Adaptor function to create predicates to check list membership with a given functor
    template <class T, class Q>
    InSetPredicateFunctor<T, Q> in_set(ObjectList<T>& list, const Functor<Q, T>& f)
    {
        ObjectList<Q> mapped_list = list.map(f);
        return InSetPredicateFunctor<T, Q>(mapped_list, f);
    }

    //! The opposite of in_set(ObjectList<T>&) adaptor
    template <class T>
    NotInSetPredicate<T> not_in_set(ObjectList<T>& list)
    {
        return NotInSetPredicate<T>(list);
    }

    //! The opposite of in_set(ObjectList<Q>&, const Functor<Q, T>&) adaptor
    template <class T, class Q>
    NotInSetPredicateFunctor<T, Q> not_in_set(ObjectList<Q>& list, const Functor<Q, T>& f)
    {
        return NotInSetPredicateFunctor<T, Q>(list, f);
    }

    //! The opposite of in_set(ObjectList<T>&, const Functor<Q, T>&) adaptor
    template <class T, class Q>
    NotInSetPredicateFunctor<T, Q> not_in_set(ObjectList<T>& list, const Functor<Q, T>& f)
    {
        ObjectList<Q> mapped_list = list.map(f);
        return NotInSetPredicateFunctor<T, Q>(mapped_list, f);
    }

    //! Convenience class to create the negate predicate of another
    template <class T>
    class NotPredicate : public Predicate<T>
    {
        private:
            const Predicate<T>& _pred;
        public:
            NotPredicate(const Predicate<T>& pred)
                : _pred(pred)
            {
            }

            virtual bool do_(typename NotPredicate::ArgType t) const
            {
                return !(_pred(t));
            }

            ~NotPredicate()
            {
            }
    };

    //! Adaptor function to create the negated predicate of another
    template <class T>
    NotPredicate<T> negate(const Predicate<T>& pred)
    {
        return NotPredicate<T>(pred);
    }
}

#endif
