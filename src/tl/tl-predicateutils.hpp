/*--------------------------------------------------------------------
  (C) Copyright 2006-2014 Barcelona Supercomputing Center
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
    //! Class of predicates useful to check whether an element is in a list
    template <class T>
    class InSetPredicate
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
            bool operator()(T t) const
            {
                return (find(_list.begin(), _list.end(), t) != _list.end());
            }
    };

    //! Class of predicates useful to check whether an element is in a list with a given comparator
    template <class T, class Q>
    class InSetPredicateFunctor
    {
        private:
            ObjectList<Q> _list;
            const std::function<Q(T)>& _f;
        public:
            //! Constructor
            /*!
             * \param list The set of elements of Q related to this predicate
             * \param f The functor type T returning type Q
             */
            InSetPredicateFunctor(ObjectList<Q>& list, const std::function<Q(T)>& f)
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
            bool operator()(T t) const
            {
                return (find(_list.begin(), _list.end(), _f(t)) != _list.end());
            }
    };

    //! The opposite of InSetPredicate
    template <class T>
    class NotInSetPredicate
    {
        private:
            InSetPredicate<T> _p;
        public:
            NotInSetPredicate(ObjectList<T>& list)
                : _p(list)
            {
            }

            bool operator()(T t) const
            {
                return !(_p(t));
            }
    };

    //! The opposite of InSetPredicateFunctor
    template <class T, class Q>
    class NotInSetPredicateFunctor
    {
        private:
            InSetPredicateFunctor<T, Q> _p;
        public:
            NotInSetPredicateFunctor(ObjectList<Q>& list, const std::function<Q(T)>& f)
                : _p(list, f)
            {
            }

            bool operator()(T t) const
            {
                return !(_p(t));
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
    InSetPredicateFunctor<T, Q> in_set(ObjectList<Q>& list, const std::function<Q(T)>& f)
    {
        return InSetPredicateFunctor<T, Q>(list, f);
    }

    //! Adaptor function to create predicates to check list membership with a given functor
    template <class T, class Q>
    InSetPredicateFunctor<T, Q> in_set(ObjectList<T>& list, const std::function<Q(T)>& f)
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
    NotInSetPredicateFunctor<T, Q> not_in_set(ObjectList<Q>& list, const std::function<Q(T)>& f)
    {
        return NotInSetPredicateFunctor<T, Q>(list, f);
    }

    //! The opposite of in_set(ObjectList<T>&, const Functor<Q, T>&) adaptor
    template <class T, class Q>
    NotInSetPredicateFunctor<T, Q> not_in_set(ObjectList<T>& list, const std::function<Q(T)>& f)
    {
        ObjectList<Q> mapped_list = list.map(f);
        return NotInSetPredicateFunctor<T, Q>(mapped_list, f);
    }
}

#endif
